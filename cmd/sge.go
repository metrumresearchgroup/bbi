package cmd

import (
	"bytes"
	"errors"
	"fmt"
	"github.com/metrumresearchgroup/babylon/utils"
	log "github.com/sirupsen/logrus"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"text/template"
	"time"

	"os"

	"github.com/metrumresearchgroup/babylon/configlib"
	"github.com/metrumresearchgroup/turnstile"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

type sgeOperation struct {
	Models []SGEModel `json:"models"`
}

//SGEModel is the struct used for SGE operations containing the NonMemModel
type SGEModel struct {
	Nonmem NonMemModel
	Cancel chan bool
}

//NewSGENonMemModel create the model details from the modelname passed
func NewSGENonMemModel(modelname string) SGEModel {
	return SGEModel{
		Nonmem: NewNonMemModel(modelname),
		Cancel: turnstile.CancellationChannel(),
	}
}

//Begin Scalable method definitions

func (l SGEModel) CancellationChannel() chan bool {
	return l.Cancel
}

//Prepare is basically the old EstimateModel function. Responsible for creating directories and preparation.
func (l SGEModel) Prepare(channels *turnstile.ChannelMap) {
	log.Debugf("%s Beginning Prepare phase of SGE Work", l.Nonmem.LogIdentifier())
	//Mark the model as started some work
	channels.Working <- 1
	fs := afero.NewOsFs()

	log.Debugf("%s Overwrite is currrently set to %t", l.Nonmem.LogIdentifier(), l.Nonmem.Configuration.Overwrite)
	log.Debugf("%s Beginning evaluation of whether or not %s exists", l.Nonmem.LogIdentifier(), l.Nonmem.OutputDir)

	//Does output directory exist?
	if ok, _ := afero.DirExists(fs, l.Nonmem.OutputDir); ok {
		//If so are we configured to overwrite?
		log.Debugf("%s Directory %s exists.", l.Nonmem.LogIdentifier(), l.Nonmem.OutputDir)

		if l.Nonmem.Configuration.Overwrite {
			log.Debugf("%s Beginning removal of %s", l.Nonmem.LogIdentifier(), l.Nonmem.OutputDir)
			err := fs.RemoveAll(l.Nonmem.OutputDir)
			if err != nil {
				RecordConcurrentError(l.Nonmem.Model, "An error occured trying to remove the directory as specified in the overwrite flag", err, channels, l.Cancel)
				return
			}
		}

		if !l.Nonmem.Configuration.Overwrite {
			log.Debugf("%s checking to see if %s contains nonmem outputs", l.Nonmem.LogIdentifier(), l.Nonmem.OutputDir)
			//If not, we only want to panic if there are nonmem output files in the directory
			if !doesDirectoryContainOutputFiles(l.Nonmem.OutputDir, l.Nonmem.FileName) {
				//Continue along if we find no relevant content
				log.Infof("%s No Nonmem output files detected in %s. Good to continue", l.Nonmem.LogIdentifier(), l.Nonmem.OutputDir)
			} else {
				//Or panic because we're in a scenario where we shouldn't purge, but there's content in the directory from previous runs
				log.Debugf("%s Configuration for overwrite was %t, but %s had Nonmem outputs. As such, we will hault operations", l.Nonmem.LogIdentifier(), l.Nonmem.Configuration.Overwrite, l.Nonmem.OutputDir)
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
	log.Debugf("%s Beginning copy operations for original model file", l.Nonmem.LogIdentifier())
	err := copyFileToDestination(l.Nonmem, true)

	//Now that the directory is created, let's create the gitignore file if specified
	if viper.GetBool("git") {
		WriteGitIgnoreFile(l.Nonmem.OutputDir)
	}

	if err != nil {
		RecordConcurrentError(l.Nonmem.Model, fmt.Sprintf("There appears to have been an issue trying to copy %s to %s", l.Nonmem.Model, l.Nonmem.OutputDir), err, channels, l.Cancel)
		return
	}

	//Create Execution Script
	scriptContents, err := generateBabylonScript(nonMemExecutionTemplate, l.Nonmem)

	if err != nil {
		RecordConcurrentError(l.Nonmem.Model, "An error occurred during the creation of the executable script for this model", err, channels, l.Cancel)
		return
	}

	//rwxr-x---
	err = afero.WriteFile(fs, path.Join(l.Nonmem.OutputDir, "grid.sh"), scriptContents, 0750)

	if viper.GetBool("debug") {
		lines, _ := utils.ReadLines(path.Join(l.Nonmem.OutputDir, "grid.sh"))
		log.Debugf("%s SGE Path's Generated Script content is \n%s", l.Nonmem.LogIdentifier(), strings.Join(lines, "\n"))
	}

	if err != nil {
		RecordConcurrentError(l.Nonmem.Model, "There was an issue writing the executable file", err, channels, l.Cancel)
	}
}

//Work describes the Turnstile execution phase -> IE What heavy lifting should be done
func (l SGEModel) Work(channels *turnstile.ChannelMap) {
	cerr := executeNonMemJob(executeSGEJob, l.Nonmem)

	if cerr.Error != nil {
		RecordConcurrentError(l.Nonmem.Model, cerr.Notes, cerr.Error, channels, l.Cancel)
		return
	}

	log.Debugf("%s Work is completed. Updating turnstile channels", l.Nonmem.LogIdentifier())
	log.Debugf("%s No monitor or cleanup phases currently exist for SGE Job. Work completed", l.Nonmem.LogIdentifier())
	channels.Completed <- 1
}

//Monitor is the 3rd phase of turnstile (not implemented here)
func (l SGEModel) Monitor(channels *turnstile.ChannelMap) {
	//Do nothing for this implementation
}

//Cleanup is the last phase of execution, in which computation / hard work is done and we're cleaning up leftover files, copying results around et all.
func (l SGEModel) Cleanup(channels *turnstile.ChannelMap) {
	//Set the config to overwrite false and re-write config. This ensures that the local phase will not deal with io contention
	//around the SGE output streams
	log.Debug("Updating babylon config to overwrite=false. This avoids IO contention with the grid engine for the next execution round")
	viper.Set("overwrite", false)
	viper.Set("saveConfig", false) //We also set saveconfig to false to avoid all of the children trying to re-save their configurations en-masse
	err := viper.WriteConfigAs(filepath.Join(l.Nonmem.OriginalPath, "babylon.yaml"))

	if err != nil {
		log.Errorf("Errors were experienced while trying to update the config at %s. Error is: %s", filepath.Join(l.Nonmem.OriginalPath, "babylon.yaml"), err)
	}
}

//End Scalable method definitions

// runCmd represents the run command
var sgeCMD = &cobra.Command{
	Use:   "sge",
	Short: "sge specifies to run a (set of) models on the Sun Grid Engine",
	Long:  runLongDescription,
	Run:   sge,
}

func init() {
	runCmd.AddCommand(sgeCMD)

	babylon, err := os.Executable()

	if err != nil {
		log.Errorf("Unable to get the path to the executed binary for some reason: %s", err)
	}

	//String Variables
	sgeCMD.PersistentFlags().String("babylonBinary", babylon, "directory path for babylon to be called in goroutines (SGE Execution)")
	viper.BindPFlag("babylonBinary", sgeCMD.PersistentFlags().Lookup("babylonBinary"))
}

func sge(cmd *cobra.Command, args []string) {

	lo := sgeOperation{}

	//Let's try to immediately load any specified configuration files
	configlib.ProcessSpecifiedConfigFile()

	//If we're in debug mode, let's set the logger to debug
	if viper.GetBool("debug") {
		log.SetLevel(log.DebugLevel)
	}

	log.Debug("Searching for models based on arguments")

	lo.Models = sgeModelsFromArguments(args)

	if len(lo.Models) == 0 {
		log.Fatal("No models were located or loaded. Please verify the arguments provided and try again")
	}

	log.Debug("Beginning config save / load operations for SGE Path")
	//Save the config file to the directory to facilitate further execution
	configlib.SaveConfig(lo.Models[0].Nonmem.OriginalPath)
	//Read it in to make sure we're not trying to save it again later
	configlib.LocateAndReadConfigFile(lo.Models[0].Nonmem.OriginalPath)

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
		log.Errorf("It appears that %d models generated an error during the initial setup phase", len(errors))
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

	log.Debug("Building turnstile manager and setting concurrency")
	//Begin Execution
	m := turnstile.NewManager(scalables, uint64(viper.GetInt("threads")))

	now := time.Now()

	log.Debug("Beginning execution")
	go m.Execute()

	//If we're in debug mode, let's periodically print out the details for the manager
	if debug {
		go func(m *turnstile.Manager) {
			for {
				log.Debugf("Manager Details: Working: %d, Errors: %d, Completed: %d, Concurrency: %d, Iterations: %d", m.Working, m.Errors, m.Completed, m.Concurrency, m.Iterations)
				time.Sleep(3 * time.Second)
			}
		}(m)
	}

	//Basically wait
	for !m.IsComplete() {
		time.Sleep(5 * time.Millisecond)
	}

	//Double check to make sure nothing has been read in before trying to write to a file
	if len(lo.Models) > 0 && viper.ConfigFileUsed() != "" {
		configlib.SaveConfig(lo.Models[0].Nonmem.OriginalPath)
	}

	postWorkNotice(m, now)
}

func newConcurrentError(model string, notes string, err error) turnstile.ConcurrentError {
	return turnstile.ConcurrentError{
		RunIdentifier: model,
		Error:         err,
		Notes:         notes,
	}
}

//RecordConcurrentError handles the processing of cancellation messages as well placing concurrent errors onto the statck
func RecordConcurrentError(model string, notes string, err error, channels *turnstile.ChannelMap, cancel chan bool) {
	cancel <- true
	channels.Errors <- newConcurrentError(model, notes, err)
}

func executeSGEJob(model NonMemModel) turnstile.ConcurrentError {
	log.Printf("%s Beginning SGE work phase", model.LogIdentifier())
	fs := afero.NewOsFs()
	//Execute the script we created

	scriptName := filepath.Join(model.OutputDir, "grid.sh")

	//Find Qsub
	binary, err := exec.LookPath("qsub")

	qsubArguments := []string{}

	qsubArguments = append(qsubArguments, []string{
		"-V",
		"-j",
		"y",
	}...)

	if model.Configuration.Parallel.Parallel {
		qsubArguments = append(qsubArguments, []string{
			"-pe",  // Parallel execution
			"orte", // Parallel environment name for the grid (Namespace for mpi messages)
			strconv.Itoa(model.Configuration.Parallel.Nodes),
		}...)
	}

	qsubArguments = append(qsubArguments, scriptName)

	if err != nil {
		return newConcurrentError(model.Model, "Could not locate qsub binary in path", err)
	}

	command := exec.Command(binary, qsubArguments...)

	//Print the whole command if we're in debug mode
	log.Debug(command.String())

	command.Env = os.Environ() //Take in OS Environment

	output, err := command.CombinedOutput()

	if err != nil {

		//Let's look to see if it's just because of the typical "No queues present" error
		if !strings.Contains(string(output), "job is not allowed to run in any queue") {
			//If the error doesn't appear to be the above error, we'll generate the concurrent error and move along
			return newConcurrentError(model.Model, "Running the programmatic shell script caused an error", err)
		}
	}

	err = afero.WriteFile(fs, path.Join(model.OutputDir, model.Model+".out"), output, 0750)

	if err != nil {
		return newConcurrentError(model.Model, "Having issues writing hte output file from command execution", err)
	}

	return turnstile.ConcurrentError{}
}

func sgeModelsFromArguments(args []string) []SGEModel {
	var output []SGEModel
	nonmemmodels := nonmemModelsFromArguments(args)

	for _, v := range nonmemmodels {
		output = append(output, SGEModel{
			Nonmem: v,
			Cancel: turnstile.CancellationChannel(),
		})
	}

	return output
}

//Generate the command line script to execute babylon on the grid.
func generateBabylonScript(fileTemplate string, l NonMemModel) ([]byte, error) {

	t, err := template.New("file").Parse(fileTemplate)
	buf := new(bytes.Buffer)
	if err != nil {
		return []byte{}, errors.New("There was an error processing the provided script template")
	}

	type content struct {
		WorkingDirectory string
		Command          string
	}

	commandComponents := []string{
		l.Configuration.BabylonBinary,
		"nonmem",
		"run",
	}

	commandComponents = append(commandComponents, []string{
		"local",
		l.Path,
	}...)

	generatedCommand := strings.TrimSpace(strings.Join(commandComponents, " "))
	log.Debugf("Generated command is %s", generatedCommand)

	//The assumption is that at this point, the config will have been written to the original path directory.

	err = t.Execute(buf, content{
		Command:          generatedCommand,
		WorkingDirectory: l.OutputDir,
	})

	if err != nil {
		return []byte{}, errors.New("An error occured during the execution of the provided script template")
	}

	return buf.Bytes(), nil
}

//Specifically used to return the name of a SGE script, modified if it begins with an integer a SGE doesn't care for that.
func getSGEScriptName(modelFileName string) string {
	r := regexp.MustCompile(`^[0-9]{1,}.*$`)

	if r.MatchString(modelFileName) {
		return "run_" + modelFileName
	}

	return modelFileName
}
