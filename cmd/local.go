package cmd

import (
	"crypto/md5"
	"encoding/json"
	"errors"
	"fmt"
	log "github.com/sirupsen/logrus"
	"io"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
	"time"

	"os"

	"github.com/metrumresearchgroup/babylon/configlib"
	"github.com/metrumresearchgroup/babylon/runner"
	"github.com/metrumresearchgroup/babylon/utils"
	"github.com/metrumresearchgroup/turnstile"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var arguments []string

type localOperation struct {
	Models []LocalModel `json:"models"`
}

//LocalModel is the struct used for local operations containing the NonMemModel
type LocalModel struct {
	Nonmem NonMemModel
	Cancel chan bool
}

//Begin Scalable method definitions

func (l LocalModel) CancellationChannel() chan bool {
	return l.Cancel
}

//Prepare is basically the old EstimateModel function. Responsible for creating directories and preparation.
func (l LocalModel) Prepare(channels *turnstile.ChannelMap) {
	log.Debugf("%s Beginning local preparation phase", l.Nonmem.LogIdentifier())

	//Jitter / Delay
	if l.Nonmem.Configuration.Delay > 0 {
		//Add a random Timer
		randomizedTimer := randomFloat(1, l.Nonmem.Configuration.Delay)

		if l.Nonmem.Configuration.Debug {
			log.Infof("Random delay of %f seconds introduced for model %s", randomizedTimer, l.Nonmem.FileName)
		}

		time.Sleep(time.Duration(randomizedTimer) * time.Second)
	}

	//Mark the model as started some work
	channels.Working <- 1

	fs := afero.NewOsFs()

	log.Debugf("%s Overwrite is currently set to %t", l.Nonmem.LogIdentifier(), viper.GetBool("debug"))
	//Does output directory exist?
	if ok, _ := afero.DirExists(fs, l.Nonmem.OutputDir); ok {
		//If so are we configured to overwrite?
		if l.Nonmem.Configuration.Overwrite {
			log.Debugf("%s Removing directory %s", l.Nonmem.LogIdentifier(), l.Nonmem.OutputDir)
			err := fs.RemoveAll(l.Nonmem.OutputDir)
			log.Error(err)
			if err != nil {
				l.Cancel <- true
				channels.Errors <- turnstile.ConcurrentError{
					RunIdentifier: l.Nonmem.Model,
					Error:         err,
					Notes:         "An error occurred trying to remove the directory as specified in the overwrite flag",
				}
				return
			}
		}

		if !l.Nonmem.Configuration.Overwrite {
			//If not, we only want to panic if there are nonmem output files in the directory
			if !doesDirectoryContainOutputFiles(l.Nonmem.OutputDir, l.Nonmem.Model) {
				//Continue along if we find no relevant content
				log.Infof("%s No Nonmem output files detected in %s. Good to continue", l.Nonmem.LogIdentifier(), l.Nonmem.OutputDir)
			} else {
				//Or panic because we're in a scenario where we shouldn't purge, but there's content in the directory from previous runs
				log.Debugf("%s Configuration for overwrite was %t, but %s had Nonmem outputs. As such, we will hault operations", l.Nonmem.LogIdentifier(), viper.GetBool("debug"), l.Nonmem.OutputDir)
				l.Cancel <- true
				channels.Errors <- turnstile.ConcurrentError{
					RunIdentifier: l.Nonmem.Model,
					Error:         errors.New("The output directory already exists"),
					Notes:         fmt.Sprintf("The target directory, %s already, exists, but we are configured to not overwrite. Invalid configuration / run state", l.Nonmem.OutputDir),
				}
				return
			}
		}

	}

	//Copy Model into destination and update Data Path
	err := copyFileToDestination(l.Nonmem, true)

	//Now that the directory is created, let's create the gitignore file if specified
	if viper.GetBool("git") {
		log.Debugf("%s Writing initial gitignore file", l.Nonmem.LogIdentifier())
		WriteGitIgnoreFile(l.Nonmem.OutputDir)
	}

	if err != nil {
		RecordConcurrentError(l.Nonmem.Model, fmt.Sprintf("There appears to have been an issue trying to copy %s to %s", l.Nonmem.Model, l.Nonmem.OutputDir), err, channels, l.Cancel)
		return
	}

	//Create Execution Script
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

	//rwxr-x---
	afero.WriteFile(fs, path.Join(l.Nonmem.OutputDir, l.Nonmem.FileName+".sh"), scriptContents, 0750)

	if l.Nonmem.Configuration.Parallel.Parallel {
		err = writeParaFile(l.Nonmem)
		if err != nil {
			log.Fatalf("%s Configuration requires parallel operation, but generation or writing of the parafile has failed: %s", l.Nonmem.LogIdentifier(), err)
		}
	}

}

//Work describes the Turnstile execution phase -> IE What heavy lifting should be done
func (l LocalModel) Work(channels *turnstile.ChannelMap) {
	cerr := executeNonMemJob(executeLocalJob, l.Nonmem)

	if cerr.Error != nil {
		RecordConcurrentError(l.Nonmem.Model, cerr.Notes, cerr.Error, channels, l.Cancel)
	}

}

//Monitor is unimplemented here. It's the 3rd phase of Turnstile execution
func (l LocalModel) Monitor(channels *turnstile.ChannelMap) {
	//Do nothing for this implementation
}

//Cleanup is the last phase of execution, in which computation / hard work is done and we're cleaning up leftover files, copying results around et all.
func (l LocalModel) Cleanup(channels *turnstile.ChannelMap) {
	time.Sleep(10 * time.Millisecond)
	log.Printf("%s Beginning cleanup phase", l.Nonmem.LogIdentifier())
	fs := afero.NewOsFs()
	// while the rest of the cleanup is happening, lets also hash the data in the background
	// so don't have to wait extra time if its on the larger end
	//Get the lines of the file
	modelPath := filepath.Join(l.Nonmem.OutputDir, l.Nonmem.Model)
	sourceLines, err := utils.ReadLines(modelPath)

	if err != nil {
		log.Errorf("%s error reading model at path: %s, to extract data path: %s\n", l.Nonmem.LogIdentifier(), modelPath, err)
	}

	log.Debugf("%s Beginning hash calculation operations for data file", l.Nonmem.LogIdentifier())

	for _, line := range sourceLines {
		if strings.Contains(line, "$DATA") {
			// extract out data path
			l.Nonmem.DataPath = filepath.Clean(strings.Fields(line)[1])
		}
	}

	hashChan := make(chan string)
	go func() {
		f, err := os.Open(l.Nonmem.DataPath)
		if err != nil {
			log.Errorf("%s error reading data to hash: %s", l.Nonmem.LogIdentifier(), err)
			hashChan <- ""
		}
		defer f.Close()

		h := md5.New()
		if _, err := io.Copy(h, f); err != nil {
			log.Errorf("%s error hashing data: %s", l.Nonmem.LogIdentifier(), err)
			hashChan <- ""
		}
		hashChan <- fmt.Sprintf("%x", h.Sum(nil))
	}()

	log.Debugf("%s Beginning selection of cleanable / copiable files", l.Nonmem.LogIdentifier())
	//Magical instructions
	//TODO: Implement flags for mandatory copy and cleanup exclusions
	pwi := newPostWorkInstruction(l.Nonmem, []string{}, []string{})

	//Copy Up first so that we don't try to move something we remove :)
	var copied []runner.TargetedFile

	log.Debugf("%s Beginning selection of copiable files ", l.Nonmem.LogIdentifier())
	for _, v := range pwi.FilesToCopy.FilesToCopy {

		source, err := utils.ReadLines(path.Join(pwi.FilesToCopy.CopyFrom, v.File))

		if err != nil {
			log.Debugf("%s Unable to read file at %s. Continuing anyway", l.Nonmem.LogIdentifier(), path.Join(pwi.FilesToCopy.CopyFrom, v.File))
			//Just continue. There are potentially files which will not exist based on the values in the list.
			continue
		}

		//Let's avoid stuttering from extension extrapolation
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
		//Write to File in original path indicating what all was copied
		copiedJSON, _ := json.MarshalIndent(copied, "", "    ")

		afero.WriteFile(fs, path.Join(l.Nonmem.OriginalPath, l.Nonmem.FileName+"_copied.json"), copiedJSON, 0750)
	}

	//Clean Up
	log.Debugf("%s Beginning local cleanup operations", l.Nonmem.LogIdentifier())
	for _, v := range pwi.FilesToClean.FilesToRemove {
		var err error

		//Does it even exist?
		if ok, _ := afero.Exists(fs, path.Join(pwi.FilesToClean.Location, v.File)); ok {
			//Is it a directory?
			if ok, _ := afero.IsDir(fs, path.Join(pwi.FilesToClean.Location, v.File)); ok {
				err = fs.RemoveAll(path.Join(pwi.FilesToClean.Location, v.File))
				//Nope it's a file!
			} else {
				err = fs.Remove(path.Join(pwi.FilesToClean.Location, v.File))
			}

			if err != nil {
				//Indicate failure to remove
				log.Errorf("%s Failure removing file / directory %s. Details : %s", l.Nonmem.LogIdentifier(), v.File, err.Error())
			}
		}
	}

	//Gitignore operations
	createNewGitIgnoreFile(l.Nonmem)

	// this should have been either completed well before, or must at least wait now to complete the hash
	// before writing out the config
	l.Nonmem.DataMD5 = <-hashChan
	//Serialize and Write the Config down to a file
	log.Debugf("%s Writing out configuration as json into %s", l.Nonmem.LogIdentifier(), l.Nonmem.OutputDir)
	err = writeNonmemConfig(l.Nonmem)

	if err != nil {
		RecordConcurrentError(l.Nonmem.FileName, "An error occurred trying to write the config file to the directory", err, channels, l.Cancel)
		return
	}

	//Mark as completed and move on to cleanup
	channels.Completed <- 1
}

//End Scalable method definitions

// runCmd represents the run command
var localCmd = &cobra.Command{
	Use:   "local",
	Short: "local specifies to run a (set of) models locally",
	Long:  runLongDescription,
	Run:   local,
}

func init() {
	runCmd.AddCommand(localCmd)
}

func local(cmd *cobra.Command, args []string) {

	log.Info("Beginning Local Path")

	//Set Logrus level if we're debug
	if viper.GetBool("debug") {
		log.SetLevel(log.DebugLevel)
	}

	//Let's try to immediately load any specified configuration files
	configlib.ProcessSpecifiedConfigFile()

	if viper.ConfigFileUsed() != "" {
		log.Infof("Config file loaded from %s", viper.ConfigFileUsed())
	}

	lo := localOperation{}

	log.Debug("Locating models from arguments")
	lo.Models = localModelsFromArguments(args)

	if len(lo.Models) == 0 {
		log.Fatal("No models were located or loaded. Please verify the arguments provided and try again")
	}

	//Display Summary

	//Models Added
	log.Infof("A total of %d models have been located for work", len(lo.Models))

	//Models in Error
	//Locate 'em
	counter := 0
	var errors []NonMemModel
	for _, v := range lo.Models {
		if v.Nonmem.Error != nil {
			counter++
			errors = append(errors, v.Nonmem)
		}
	}

	if counter > 0 {
		log.Infof("It appears that %d models generated an error during the initial setup phase", len(errors))
		for _, v := range errors {
			log.Errorf("Model named %s has errored. Details: %s", v.Model, v.Error.Error())
		}
	}

	//Models in OK state
	log.Infof("%d models successfully completed initial setup phase.", len(lo.Models)-len(errors))

	//Create signature safe slice for manager
	var scalables []turnstile.Scalable

	for _, v := range lo.Models {
		scalables = append(scalables, v)
	}

	//Begin Execution
	log.Debug("Building turnstile manager")
	m := turnstile.NewManager(scalables, uint64(viper.GetInt("threads")))

	now := time.Now()

	log.Debug("Beginning turnstile execution")
	go m.Execute()

	//If we're in debug mode, let's periodically print out the details for the manager
	if viper.GetBool("debug") {
		go func(m *turnstile.Manager) {
			for {
				log.Infof("Manager Details: Working: %d, Errors: %d, Completed: %d, Concurrency: %d, Iterations: %d", m.Working, m.Errors, m.Completed, m.Concurrency, m.Iterations)
				time.Sleep(3 * time.Second)
			}
		}(m)
	}

	//Basically wait
	for !m.IsComplete() {
		time.Sleep(5 * time.Millisecond)
	}

	if len(lo.Models) > 0 {
		if viper.GetBool("saveConfig") {
			configlib.SaveConfig(lo.Models[0].Nonmem.OriginalPath)
		}
	}

	postWorkNotice(m, now)
}

//WriteGitIgnoreFile takes a provided path and does best attempt work to write a "Exclude all" gitignore file in the location
func WriteGitIgnoreFile(filepath string) {
	utils.WriteLines([]string{"*"}, path.Join(filepath, ".gitignore"))
}

func executeLocalJob(model NonMemModel) turnstile.ConcurrentError {
	log.Infof("%s Beginning local work phase", model.LogIdentifier())
	fs := afero.NewOsFs()

	scriptLocation := path.Join(model.OutputDir, model.FileName+".sh")
	os.Chdir(model.OutputDir)

	command := exec.Command(scriptLocation)
	command.Env = os.Environ() //Take in OS Environment

	log.Debugf("%s Generated command was: %s", model.LogIdentifier(), command.String())

	output, err := command.CombinedOutput()

	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			code := exitError.ExitCode()
			details := exitError.String()

			log.Errorf("%s Exit code was %d, details were %s", model.LogIdentifier(), code, details)
			log.Errorf("%s output details were: %s", model.LogIdentifier(), string(output))
		}
		return newConcurrentError(model.Model, "Running the programmatic shell script caused an error", err)

	}

	afero.WriteFile(fs, path.Join(model.OutputDir, model.Model+".out"), output, 0750)

	return turnstile.ConcurrentError{}
}

func localModelsFromArguments(args []string) []LocalModel {
	var output []LocalModel
	nonmemmodels := nonmemModelsFromArguments(args)

	for _, v := range nonmemmodels {
		output = append(output, LocalModel{
			Nonmem: v,
			Cancel: turnstile.CancellationChannel(),
		})
	}

	return output
}

func createNewGitIgnoreFile(m NonMemModel) error {
	log.Debugf("%s Writing finalized gitignore file", m.LogIdentifier())
	//First let's remove the gitignore in the output dir.
	fs := afero.NewOsFs()
	if ok, _ := afero.Exists(fs, path.Join(m.OutputDir, ".gitignore")); ok {
		//If the gitignore file exists let's remove it
		fs.Remove(path.Join(m.OutputDir, ".gitignore"))
	}

	//Force level one per initial discussions
	linesToAddToGitignore := getCleanableFileList(m.FileName, 1)

	err := utils.WriteLines(linesToAddToGitignore, path.Join(m.OutputDir, ".gitignore"))

	if err != nil {
		return err
	}

	return nil
}

func writeNonmemConfig(model NonMemModel) error {
	outBytes, err := json.MarshalIndent(model, "", "    ")

	if err != nil {
		return err
	}

	return afero.WriteFile(afero.NewOsFs(), path.Join(model.OutputDir, "bbi_config.json"), outBytes, 0750)
}
