package cmd

import (
	"bbi/utils"
	"bytes"
	"errors"
	log "github.com/sirupsen/logrus"
	"os/exec"
	"path"
	"path/filepath"
	"strconv"
	"strings"
	"text/template"
	"time"

	"os"

	"bbi/configlib"
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
	Nonmem               *NonMemModel
	Cancel               chan bool
	postworkInstructions *PostExecutionHookEnvironment
}

func (s *SGEModel) BuildExecutionEnvironment(completed bool, err error) {
	s.postworkInstructions = &PostExecutionHookEnvironment{
		ExecutionBinary: s.Nonmem.Configuration.PostWorkExecutable,
		ModelPath:       s.Nonmem.Path,
		Model:           s.Nonmem.Model,
		Filename:        s.Nonmem.FileName,
		Extension:       s.Nonmem.Extension,
		OutputDirectory: s.Nonmem.Configuration.OutputDir,
		Successful:      completed,
		Error:           err,
	}
}

func (s *SGEModel) GetPostWorkConfig() *PostExecutionHookEnvironment {
	return s.postworkInstructions
}

func (s *SGEModel) GetPostWorkExecutablePath() string {
	return s.Nonmem.Configuration.PostWorkExecutable
}

func (s *SGEModel) GetGlobalConfig() configlib.Config {
	return s.Nonmem.Configuration
}

func (s *SGEModel) GetWorkingPath() string {
	return s.Nonmem.OutputDir
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

	err := createChildDirectories(l.Nonmem, l.Cancel, channels, true)

	//Save the config into the output directory

	if err != nil {
		//Handles the cancel operation
		p := &l
		p.BuildExecutionEnvironment(false, err)
		RecordConcurrentError(p.Nonmem.FileName, err.Error(), err, channels, p.Cancel, p)
		return
	}

	//Create Execution Script
	scriptContents, err := generateBbiScript(nonMemExecutionTemplate, *l.Nonmem)

	if err != nil {
		p := &l
		p.BuildExecutionEnvironment(false, err)
		RecordConcurrentError(p.Nonmem.Model, "An error occurred during the creation of the executable script for this model", err, channels, p.Cancel, p)
		return
	}

	//rwxr-x---
	err = afero.WriteFile(fs, path.Join(l.Nonmem.OutputDir, "grid.sh"), scriptContents, 0750)

	if viper.GetBool("debug") {
		lines, _ := utils.ReadLines(path.Join(l.Nonmem.OutputDir, "grid.sh"))
		log.Debugf("%s SGE Path's Generated Script content is \n%s", l.Nonmem.LogIdentifier(), strings.Join(lines, "\n"))
	}

	if err != nil {
		p := &l
		p.BuildExecutionEnvironment(false, err)
		RecordConcurrentError(p.Nonmem.Model, "There was an issue writing the executable file", err, channels, p.Cancel, p)
	}
}

//Work describes the Turnstile execution phase -> IE What heavy lifting should be done
func (l SGEModel) Work(channels *turnstile.ChannelMap) {
	cerr := executeNonMemJob(executeSGEJob, l.Nonmem)

	if cerr.Error != nil {
		p := &l
		p.BuildExecutionEnvironment(false, cerr.Error)
		RecordConcurrentError(p.Nonmem.Model, cerr.Notes, cerr.Error, channels, p.Cancel, p)
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
	//err := configlib.WriteViperConfig(l.Nonmem.OutputDir, true)
	//
	//if err != nil {
	//	log.Errorf("Errors were experienced while trying to update the config at %s. Error is: %s", filepath.Join(l.Nonmem.OriginalPath, "bbi.yaml"), err)
	//}
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

	bbi, err := os.Executable()

	if err != nil {
		log.Errorf("Unable to get the path to the executed binary for some reason: %s", err)
	}

	//String Variables
	sgeCMD.PersistentFlags().String("bbi_binary", bbi, "directory path for bbi to be called in goroutines (SGE Execution)")
	viper.BindPFlag("bbi_binary", sgeCMD.PersistentFlags().Lookup("bbi_binary"))

	const gridNamePrefixIdentifier string = "grid_name_prefix"
	sgeCMD.PersistentFlags().String(gridNamePrefixIdentifier, "", "Any prefix you wish to add to the name of jobs being submitted to the grid")
	viper.BindPFlag(gridNamePrefixIdentifier, sgeCMD.PersistentFlags().Lookup(gridNamePrefixIdentifier))
}

func sge(cmd *cobra.Command, args []string) {

	config, err := configlib.LocateAndReadConfigFile()
	if err != nil {
		log.Fatalf("Failed to process configuration: %s", err)
	}

	log.Info("Beginning Local Path")

	lo := sgeOperation{}

	logSetup(config)

	log.Debug("Searching for models based on arguments")
	lomodels, err := sgeModelsFromArguments(args, config)
	if err != nil {
		log.Fatalf("An error occurred during model processing: %s", err)
	}

	lo.Models = lomodels

	if len(lo.Models) == 0 {
		log.Fatal("No models were located or loaded. Please verify the arguments provided and try again")
	}

	//Models Added
	log.Infof("A total of %d models have been located for work", len(lo.Models))

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

	if len(m.ErrorList) > 0 {
		os.Exit(1)
	}
}

func newConcurrentError(model string, notes string, err error) turnstile.ConcurrentError {
	return turnstile.ConcurrentError{
		RunIdentifier: model,
		Error:         err,
		Notes:         notes,
	}
}

func executeSGEJob(model *NonMemModel) turnstile.ConcurrentError {
	log.Printf("%s Beginning SGE work phase", model.LogIdentifier())
	fs := afero.NewOsFs()
	//Execute the script we created

	//Compute the grid name for submission
	submittedName, err := gridengineJobName(model)

	if err != nil {
		return newConcurrentError(model.FileName, "Failed to template out name for job submission", err)
	}

	scriptName := "grid.sh"

	//Find Qsub
	binary, err := exec.LookPath("qsub")

	qsubArguments := []string{}

	qsubArguments = append(qsubArguments, []string{
		"-V",
		"-j",
		"y",
		"-N",
		submittedName,
	}...)

	if model.Configuration.Parallel {
		qsubArguments = append(qsubArguments, []string{
			"-pe",  // Parallel execution
			"orte", // Parallel environment name for the grid (Namespace for mpi messages)
			strconv.Itoa(model.Configuration.Threads),
		}...)
	}

	qsubArguments = append(qsubArguments, filepath.Join(model.OutputDir, scriptName))

	if err != nil {
		return newConcurrentError(model.Model, "Could not locate qsub binary in path", err)
	}

	command := exec.Command(binary, qsubArguments...)

	//Print the whole command if we're in debug mode
	log.Debugf("QSUB command is:%s", command.String())

	command.Env = os.Environ() //Take in OS Environment

	additionalEnvs := model.Configuration.GetPostWorkExecEnvs()

	if len(additionalEnvs) > 0 {
		log.Debugf("Additional post work envs were provided. Total of %d", len(additionalEnvs))
		command.Env = append(command.Env, additionalEnvs...)
	}

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

func sgeModelsFromArguments(args []string, config configlib.Config) ([]SGEModel, error) {
	var output []SGEModel
	nonmemmodels, err := nonmemModelsFromArguments(args, config)

	if err != nil {
		return output, err
	}

	for _, v := range nonmemmodels {
		n := v
		output = append(output, SGEModel{
			Nonmem: &n,
			Cancel: turnstile.CancellationChannel(),
		})
	}

	return output, nil
}

//Generate the command line script to execute bbi on the grid.
func generateBbiScript(fileTemplate string, l NonMemModel) ([]byte, error) {

	t, err := template.New("file").Parse(fileTemplate)
	buf := new(bytes.Buffer)
	if err != nil {
		return []byte{}, errors.New("There was an error processing the provided script template")
	}

	filename := l.Model

	if l.Configuration.NMQual {
		filename = l.FileName + ".ctl"
	}

	type content struct {
		WorkingDirectory string
		Command          string
	}

	commandComponents := []string{
		l.Configuration.BbiBinary,
		"nonmem",
		"run",
	}

	commandComponents = append(commandComponents, []string{
		"local",
		filename,
	}...)

	if !l.Configuration.Local.CreateChildDirs {
		commandComponents = append(commandComponents, []string{
			"--create_child_dirs=false",
		}...)
	}

	generatedCommand := strings.TrimSpace(strings.Join(commandComponents, " "))
	log.Debugf("Generated command is %s", generatedCommand)

	err = t.Execute(buf, content{
		Command:          generatedCommand,
		WorkingDirectory: l.OutputDir,
	})

	if err != nil {
		return []byte{}, errors.New("An error occured during the execution of the provided script template")
	}

	return buf.Bytes(), nil
}

func gridengineJobName(model *NonMemModel) (string, error) {
	templateString := `{{ if .Prefix }}{{ .Prefix }}_Run_{{ .Filename }}{{else}}Run_{{ .Filename }}{{end}}`
	t, err := template.New("run_name").Parse(templateString)

	if err != nil {
		return "", err
	}

	type templateContent struct {
		Prefix   string
		Filename string
	}

	outBytesBuffer := new(bytes.Buffer)

	err = t.Execute(outBytesBuffer, templateContent{
		Prefix:   model.Configuration.GridNamePrefix,
		Filename: model.FileName,
	})

	if err != nil {
		return "", err
	}

	return outBytesBuffer.String(), nil
}
