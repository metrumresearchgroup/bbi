package cmd

import (
	// TODO: use a better md5.
	"crypto/md5" // nolint:gosec
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
	"time"

	"github.com/metrumresearchgroup/bbi/configlib"

	log "github.com/sirupsen/logrus"

	"os"

	"github.com/metrumresearchgroup/bbi/runner"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/turnstile"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

type localOperation struct {
	Models []LocalModel `json:"models"`
}

// LocalModel is the struct used for local operations containing the NonMemModel.
type LocalModel struct {
	Nonmem               *NonMemModel
	Cancel               chan bool
	postworkInstructions *PostExecutionHookEnvironment
}

func (l *LocalModel) BuildExecutionEnvironment(completed bool, err error) {
	l.postworkInstructions = &PostExecutionHookEnvironment{
		ExecutionBinary: l.Nonmem.Configuration.PostWorkExecutable,
		ModelPath:       l.Nonmem.Path,
		Model:           l.Nonmem.Model,
		Filename:        l.Nonmem.FileName,
		Extension:       l.Nonmem.Extension,
		OutputDirectory: l.Nonmem.OutputDir,
		Successful:      completed,
		Error:           err,
	}
}

func (l *LocalModel) GetPostWorkConfig() *PostExecutionHookEnvironment {
	return l.postworkInstructions
}

func (l *LocalModel) GetPostWorkExecutablePath() string {
	return l.Nonmem.Configuration.PostWorkExecutable
}

func (l *LocalModel) GetGlobalConfig() configlib.Config {
	return l.Nonmem.Configuration
}

func (l *LocalModel) GetWorkingPath() string {
	return l.Nonmem.OutputDir
}

// Begin Scalable method definitions.
func (l LocalModel) CancellationChannel() chan bool {
	return l.Cancel
}

// Prepare is basically the old EstimateModel function. Responsible for creating directories and preparation.
func (l LocalModel) Prepare(channels *turnstile.ChannelMap) {
	// Mark the model as started some work
	channels.Working <- 1

	log.Debugf("%s Beginning local preparation phase", l.Nonmem.LogIdentifier())

	// Check for invalid selected versions of Nonmem if NMQual selected
	if l.Nonmem.Configuration.NMQual {
		// Set parallelism to true
		l.Nonmem.Configuration.Parallel = true
		// TODO: What about the other parallel components?

		// If we can locate the key
		if selected, ok := l.Nonmem.Configuration.Nonmem[l.Nonmem.Configuration.NMVersion]; ok {
			// Is it not set to nmqual?
			if !selected.Nmqual {
				reportedError := errors.New("NMQual was selected, but the selected nmversion does not support nmqual")
				// Build the post execution environment
				p := &l
				p.BuildExecutionEnvironment(false, reportedError)
				RecordConcurrentError(p.Nonmem.FileName, "Invalid nonmem / nmqual configuration", reportedError, channels, p.Cancel, p)

				return
			}
		}
	}

	// Jitter / Delay
	if l.Nonmem.Configuration.Delay > 0 {
		// Add a random Timer
		randomizedTimer := randomFloat(1, l.Nonmem.Configuration.Delay)

		if l.Nonmem.Configuration.Debug {
			log.Infof("Random delay of %f seconds introduced for model %s", randomizedTimer, l.Nonmem.FileName)
		}

		time.Sleep(time.Duration(randomizedTimer) * time.Second)
	}

	fs := afero.NewOsFs()

	if !l.Nonmem.Configuration.Local.CreateChildDirs {
		log.Debugf("Create Child Dir directive is %t. Setting Model output dir = %s", l.Nonmem.Configuration.Local.CreateChildDirs, l.Nonmem.OriginalPath)
		// Set output dir to Original Path for execution sake
		l.Nonmem.OutputDir = l.Nonmem.OriginalPath
	}

	if l.Nonmem.Configuration.Local.CreateChildDirs {
		err := createChildDirectories(l.Nonmem, false)

		if err != nil {
			// Handles the cancel operation
			p := &l // Use the pointer from here to manage value manipulations
			p.BuildExecutionEnvironment(false, err)
			RecordConcurrentError(p.Nonmem.FileName, err.Error(), err, channels, p.Cancel, p)

			return
		}
	}

	// Now that we've copied the files, let's make sure we reflect a .ctl file if we're in nmqual operational mode
	if l.Nonmem.Configuration.NMQual {
		log.Debugf("%s since we're in nmqual mode operationally, we're going to change the model from "+
			"%s to %s ", l.Nonmem.LogIdentifier(), l.Nonmem.Model, l.Nonmem.FileName+".ctl")
		l.Nonmem.Model = l.Nonmem.FileName + ".ctl"
	}

	// Create Execution Script
	log.Debugf("%s Creating local execution script", l.Nonmem.LogIdentifier())
	scriptContents, err := generateScript(nonMemExecutionTemplate, l.Nonmem)

	if err != nil {
		l.Cancel <- true
		channels.Errors <- turnstile.ConcurrentError{
			RunIdentifier: l.Nonmem.Model,
			Error:         err,
			Notes:         "An error occurred during the creation of the executable script for this model",
		}

		return
	}

	log.Debugf("%s Writing script to file", l.Nonmem.LogIdentifier())

	// rwxr-x---
	if err = afero.WriteFile(fs, path.Join(l.Nonmem.OutputDir, l.Nonmem.FileName+".sh"), scriptContents, 0750); err != nil {
		channels.Errors <- turnstile.ConcurrentError{
			RunIdentifier: l.Nonmem.FileName,
			Notes:         "writing the .sh file",
			Error:         err,
		}
	}

	if l.Nonmem.Configuration.Parallel {
		if err = writeParaFile(l.Nonmem); err != nil {
			channels.Errors <- turnstile.ConcurrentError{
				RunIdentifier: l.Nonmem.FileName,
				Notes:         "writing to the parafile",
				Error:         err,
			}
			//			log.Fatalf("%s Configuration requires parallel operation, but generation or writing of the parafile has failed: %s", l.Nonmem.LogIdentifier(), err)
		}
	}
}

// Work describes the Turnstile execution phase -> IE What heavy lifting should be done.
func (l LocalModel) Work(channels *turnstile.ChannelMap) {
	cerr := executeNonMemJob(executeLocalJob, l.Nonmem)

	if cerr.Error != nil {
		p := &l
		p.BuildExecutionEnvironment(false, cerr.Error)
		RecordConcurrentError(p.Nonmem.Model, cerr.Notes, cerr.Error, channels, p.Cancel, p)
	}
}

// Monitor is unimplemented here. It's the 3rd phase of Turnstile execution.
func (l LocalModel) Monitor(_ *turnstile.ChannelMap) {
	// Do nothing for this implementation
}

// Cleanup is the last phase of execution, in which computation / hard work is done and we're cleaning up leftover files, copying results around et all.
func (l LocalModel) Cleanup(channels *turnstile.ChannelMap) {
	time.Sleep(10 * time.Millisecond)
	log.Printf("%s Beginning cleanup phase", l.Nonmem.LogIdentifier())
	fs := afero.NewOsFs()
	// while the rest of the cleanup is happening, lets also hash the data in the background
	// so don't have to wait extra time if its on the larger end
	// Get the lines of the file
	modelPath := filepath.Join(l.Nonmem.OutputDir, l.Nonmem.Model)
	sourceLines, err := utils.ReadLines(modelPath)

	if err != nil {
		log.Errorf("%s error reading model at path: %s, to extract data path: %s\n", l.Nonmem.LogIdentifier(), modelPath, err)
	}

	log.Debugf("%s Beginning hash calculation operations for data file", l.Nonmem.LogIdentifier())

	for _, line := range sourceLines {
		if strings.HasPrefix(strings.TrimSpace(line), "$DATA") {
			// extract out data path
			l.Nonmem.DataPath = filepath.Clean(strings.Fields(line)[1])
		}
	}

	dataHashChan := make(chan string)
	go HashFileOnChannel(dataHashChan, path.Join(l.Nonmem.OutputDir, l.Nonmem.DataPath), l.Nonmem.FileName)

	modelHashChan := make(chan string)
	go HashFileOnChannel(modelHashChan, path.Join(l.Nonmem.OriginalPath, l.Nonmem.OriginalModel), l.Nonmem.FileName)

	log.Debugf("%s Beginning selection of cleanable / copiable files", l.Nonmem.LogIdentifier())
	// Magical instructions
	// TODO: Implement flags for mandatory copy and cleanup exclusions
	pwi := newPostWorkInstruction(l.Nonmem, []string{}, []string{})

	// Copy Up first so that we don't try to move something we remove :)
	var copied []runner.TargetedFile

	log.Debugf("%s Beginning selection of copiable files ", l.Nonmem.LogIdentifier())
	for _, v := range pwi.FilesToCopy.FilesToCopy {
		var source []string
		source, err = utils.ReadLines(path.Join(pwi.FilesToCopy.CopyFrom, v.File))

		if err != nil {
			log.Debugf("%s Unable to read file at %s. Continuing anyway", l.Nonmem.LogIdentifier(), path.Join(pwi.FilesToCopy.CopyFrom, v.File))
			// Just continue. There are potentially files which will not exist based on the values in the list.
			continue
		}

		// Let's avoid stuttering from extension extrapolation
		file, _ := utils.FileAndExt(v.File)

		if file == l.Nonmem.FileName {
			file = v.File
		} else {
			file = l.Nonmem.FileName + "." + v.File
		}

		err = utils.WriteLines(source, path.Join(pwi.FilesToCopy.CopyTo, file))

		if err != nil {
			log.Errorf("%s An error occurred while attempting to copy the files: File is %s", l.Nonmem.LogIdentifier(), v.File)

			continue
		}

		v.File = filepath.Join(l.Nonmem.OutputDir, v.File)

		copied = append(copied, v)
	}

	if len(copied) > 0 {
		log.Debugf("%s Writing out copied json file for", l.Nonmem.LogIdentifier())
		// Write to File in original path indicating what all was copied
		copiedJSON, _ := json.MarshalIndent(copied, "", "    ")

		if err = afero.WriteFile(fs, path.Join(l.Nonmem.OriginalPath, l.Nonmem.FileName+"_copied.json"), copiedJSON, 0750); err != nil {
			channels.Errors <- turnstile.ConcurrentError{
				RunIdentifier: l.Nonmem.FileName,
				Notes:         "could not write _copied.json file",
				Error:         err,
			}
		}
	}

	// Clean Up
	log.Debugf("%s Beginning local cleanup operations", l.Nonmem.LogIdentifier())
	for _, v := range pwi.FilesToClean.FilesToRemove {
		// Does it even exist?
		if ok, _ := afero.Exists(fs, path.Join(pwi.FilesToClean.Location, v.File)); ok {
			// Is it a directory?
			if ok, _ := afero.IsDir(fs, path.Join(pwi.FilesToClean.Location, v.File)); ok {
				err = fs.RemoveAll(path.Join(pwi.FilesToClean.Location, v.File))
				// Nope it's a file!
			} else {
				err = fs.Remove(path.Join(pwi.FilesToClean.Location, v.File))
			}

			if err != nil {
				// Indicate failure to remove
				log.Errorf("%s Failure removing file / directory %s. Details : %s", l.Nonmem.LogIdentifier(), v.File, err.Error())
			}
		}
	}

	// Gitignore operations
	if err = createNewGitIgnoreFile(l.Nonmem); err != nil {
		channels.Errors <- turnstile.ConcurrentError{
			RunIdentifier: l.Nonmem.FileName,
			Notes:         "failed writing .gitignore file ",
			Error:         err,
		}
	}

	// this should have been either completed well before, or must at least wait now to complete the hash
	// before writing out the config
	l.Nonmem.DataMD5 = <-dataHashChan
	l.Nonmem.ModelMD5 = <-modelHashChan

	// Serialize and Write the Config down to a file
	log.Debugf("%s Writing out configuration as json into %s", l.Nonmem.LogIdentifier(), l.Nonmem.OutputDir)
	err = writeNonmemConfig(l.Nonmem)

	if err != nil {
		p := &l
		p.BuildExecutionEnvironment(false, err)
		RecordConcurrentError(p.Nonmem.FileName, "An error occurred trying to write the config file to the directory", err, channels, p.Cancel, p)

		return
	}

	log.Debugf("Post Work Executable set as %s", l.Nonmem.Configuration.PostWorkExecutable)

	/*

		Post Execution Instructions

	*/

	// PostWorkExecution phase if the script value is not empty

	PostWorkExecution(&l, true, nil)

	log.Infof("%s Cleanup completed", l.Nonmem.LogIdentifier())
	channels.Completed <- 1
}

// End Scalable method definitions

func NewLocalCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:     "local [flags] <model> [<model>...]",
		Short:   "Run models locally",
		Example: fmt.Sprintf(runExamples, "local"),
		Run:     local,
	}

	childDirIdentifier := "create_child_dirs"
	cmd.PersistentFlags().Bool(childDirIdentifier, true,
		"create a new subdirectory, named based on the output_dir option, for execution")
	errpanic(viper.BindPFlag("local."+childDirIdentifier, cmd.PersistentFlags().Lookup(childDirIdentifier)))

	return cmd
}

func local(_ *cobra.Command, args []string) {
	config, err := configlib.LocateAndReadConfigFile()

	if err != nil {
		log.Fatalf("Failed to process configuration: %s", err)
	}

	log.Info("Beginning Local Path")

	logSetup(config)

	lo := localOperation{}

	log.Debug("Locating models from arguments")
	localmodels, err := localModelsFromArguments(args, config)

	if err != nil {
		log.Fatalf("An error occurred during model processing: %s", err)
	}

	lo.Models = localmodels

	if len(lo.Models) == 0 {
		log.Fatal("No models were located or loaded. Please verify the arguments provided and try again")
	}

	// Models Added
	log.Infof("A total of %d models have completed the initial preparation phase", len(lo.Models))

	// Create signature safe slice for manager
	var scalables []turnstile.Scalable

	for _, v := range lo.Models {
		scalables = append(scalables, v)
	}

	// Begin Execution
	log.Debug("Building turnstile manager")
	m := turnstile.NewManager(scalables, uint64(viper.GetInt("threads")))

	now := time.Now()

	log.Debug("Beginning turnstile execution")
	go m.Execute()

	// If we're in debug mode, let's periodically print out the details for the manager
	if viper.GetBool("debug") {
		go func(m *turnstile.Manager) {
			for {
				log.Infof("Manager Details: Working: %d, Errors: %d, Completed: %d, Concurrency: %d, Iterations: %d", m.Working, m.Errors, m.Completed, m.Concurrency, m.Iterations)
				time.Sleep(3 * time.Second)
			}
		}(m)
	}

	// Basically wait
	for !m.IsComplete() {
		time.Sleep(5 * time.Millisecond)
	}

	postWorkNotice(m, now)

	if len(m.ErrorList) > 0 {
		os.Exit(1)
	}
}

// WriteGitIgnoreFile takes a provided path and does best attempt work to write a "Exclude all" gitignore file in the location.
func WriteGitIgnoreFile(filepath string) error {
	return utils.WriteLines([]string{"*"}, path.Join(filepath, ".gitignore"))
}

func executeLocalJob(model *NonMemModel) turnstile.ConcurrentError {
	log.Infof("%s Beginning local work phase", model.LogIdentifier())
	fs := afero.NewOsFs()

	log.Debugf("Output directory is currently set to %s", model.OutputDir)

	scriptLocation := path.Join(model.OutputDir, model.FileName+".sh")

	log.Debugf("Script location is pegged at %s", scriptLocation)

	bash, err := exec.LookPath("bash")
	if err != nil {
		return turnstile.ConcurrentError{
			Error:         err,
			RunIdentifier: model.FileName,
			Notes:         "bash is required to run bbi. Please install bash and then try running this again.",
		}
	}

	command := exec.Command(bash, scriptLocation)
	command.Dir = model.OutputDir
	command.Env = os.Environ() // Take in OS Environment

	log.Debugf("%s Generated command was: %s", model.LogIdentifier(), command.String())

	output, err := command.CombinedOutput()

	if err != nil && !strings.Contains(string(output), "not well-formed (invalid token)") {
		log.Debug(err)

		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			code := exitError.ExitCode()
			details := exitError.String()

			log.Errorf("%s Exit code was %d, details were %s", model.LogIdentifier(), code, details)
			log.Errorf("%s output details were: %s", model.LogIdentifier(), string(output))
		}

		return newConcurrentError(model.Model, "Running the programmatic shell script caused an error", err)
	}

	if err = afero.WriteFile(fs, path.Join(model.OutputDir, model.Model+".out"), output, 0750); err != nil {
		return turnstile.ConcurrentError{Error: err, Notes: "unable to write model to output directory", RunIdentifier: model.FileName}
	}

	return turnstile.ConcurrentError{}
}

func localModelsFromArguments(args []string, config configlib.Config) ([]LocalModel, error) {
	var output []LocalModel
	nonmemmodels, err := nonmemModelsFromArguments(args, config)

	if err != nil {
		return output, err
	}

	for _, v := range nonmemmodels {
		// Creating a copy of it here to avoid duplicate memory references
		n := v

		output = append(output, LocalModel{
			Nonmem: &n,
			Cancel: turnstile.CancellationChannel(),
		})
	}

	return output, nil
}

func createNewGitIgnoreFile(m *NonMemModel) error {
	log.Debugf("%s Writing finalized gitignore file", m.LogIdentifier())
	// First let's remove the gitignore in the output dir.
	fs := afero.NewOsFs()
	if ok, _ := afero.Exists(fs, path.Join(m.OutputDir, ".gitignore")); ok {
		// If the gitignore file exists let's remove it
		if err := fs.Remove(path.Join(m.OutputDir, ".gitignore")); err != nil {
			_, _ = fmt.Fprintf(os.Stderr, "could not remove .gitignore file: %s", err)
		}
	}

	// Force level one per initial discussions
	linesToAddToGitignore := getCleanableFileList(m.FileName, 1)
	linesToAddToGitignore = append(linesToAddToGitignore, "WK_[0-9]*")

	err := utils.WriteLines(linesToAddToGitignore, path.Join(m.OutputDir, ".gitignore"))

	if err != nil {
		return err
	}

	return nil
}

func writeNonmemConfig(model *NonMemModel) error {
	outBytes, err := json.MarshalIndent(model, "", "    ")

	if err != nil {
		return err
	}

	return afero.WriteFile(afero.NewOsFs(), path.Join(model.OutputDir, "bbi_config.json"), outBytes, 0750)
}

func HashFileOnChannel(ch chan string, file string, identifier string) {
	f, err := os.Open(file)
	if err != nil {
		log.Debugf("File requested was %s, Identifier is %s", file, identifier)
		log.Errorf("%s error reading data to hash: %s", identifier, err)
		ch <- ""
	}
	defer f.Close()

	// TODO: find a better md5
	h := md5.New() // nolint:gosec
	if _, err := io.Copy(h, f); err != nil {
		log.Errorf("%s error hashing data: %s", identifier, err)
		ch <- ""
	}
	ch <- fmt.Sprintf("%x", h.Sum(nil))
}
