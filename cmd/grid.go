package cmd

import (
	"bytes"
	"fmt"
	"os/exec"
	"path/filepath"
	"strings"
	"text/template"
	"time"

	"github.com/metrumresearchgroup/bbi/utils"

	log "github.com/sirupsen/logrus"

	"os"

	"github.com/metrumresearchgroup/bbi/configlib"

	"github.com/metrumresearchgroup/turnstile"
	"github.com/spf13/afero"
	"github.com/spf13/viper"
)

// gridSpec defines how to submit a model to the grid.
type gridSpec struct {
	// Name is a label for the type of submission (e.g., "SGE" or "Slurm") for
	// use in log messages.
	Name string
	// SubmitCommand is the name of the program used to submit to the grid
	// (e.g., "qsub" or "sbatch").  It must accept a submission script as its
	// one and only positional argument.
	SubmitCommand string
	// Template is a template used to generate the grid submission script.  It
	// is applied to a data object with the following fields:
	//
	//  * Command (string): the shell-escaped command to execute
	//
	//  * Config (configlib.Config): the active configuration, which is useful
	//    for inspecting settings like Parallel and Threads.
	//
	//  * JobName (string): the name that should be used for the grid submission
	//
	//  * WorkingDirectory (string): the directory in which the command should be
	//    executed
	Template string
	// IgnoreError is a function called if submitting the script fails.  It
	// takes the error and combined standard output and standard error as
	// arguments.  A return value of true indicates that the error should be
	// swallowed rather propagated as a turnstile.ConcurrentError.
	IgnoreError func(error, string) bool
}

// gridModel is the struct used for grid operations.
type gridModel struct {
	Spec                 *gridSpec
	Nonmem               *NonMemModel
	Cancel               chan bool
	postworkInstructions *PostExecutionHookEnvironment
}

func (g *gridModel) BuildExecutionEnvironment(completed bool, err error) {
	g.postworkInstructions = &PostExecutionHookEnvironment{
		ExecutionBinary: g.Nonmem.Configuration.PostWorkExecutable,
		ModelPath:       g.Nonmem.Path,
		Model:           g.Nonmem.Model,
		Filename:        g.Nonmem.FileName,
		Extension:       g.Nonmem.Extension,
		OutputDirectory: g.Nonmem.Configuration.OutputDir,
		Successful:      completed,
		Error:           err,
	}
}

func (g *gridModel) GetPostWorkConfig() *PostExecutionHookEnvironment {
	return g.postworkInstructions
}

func (g *gridModel) GetPostWorkExecutablePath() string {
	return g.Nonmem.Configuration.PostWorkExecutable
}

func (g *gridModel) GetGlobalConfig() configlib.Config {
	return g.Nonmem.Configuration
}

func (g *gridModel) GetWorkingPath() string {
	return g.Nonmem.OutputDir
}

// Begin Scalable method definitions.
func (g gridModel) CancellationChannel() chan bool {
	return g.Cancel
}

// Prepare is basically the old EstimateModel function. Responsible for creating directories and preparation.
func (g gridModel) Prepare(channels *turnstile.ChannelMap) {
	log.Debugf("%s Beginning Prepare phase of %s Work", g.Nonmem.LogIdentifier(), g.Spec.Name)

	//Mark the model as started some work
	channels.Working <- 1

	fs := afero.NewOsFs()

	log.Debugf("%s Overwrite is currrently set to %t", g.Nonmem.LogIdentifier(), g.Nonmem.Configuration.Overwrite)
	log.Debugf("%s Beginning evaluation of whether or not %s exists", g.Nonmem.LogIdentifier(), g.Nonmem.OutputDir)

	err := createChildDirectories(g.Nonmem, true)

	//Save the config into the output directory

	if err != nil {
		//Handles the cancel operation
		p := &g
		p.BuildExecutionEnvironment(false, err)
		RecordConcurrentError(p.Nonmem.FileName, err.Error(), err, channels, p.Cancel, p)

		return
	}

	//Create Execution Script
	scriptContents, err := generateBbiScript(g.Spec.Template, *g.Nonmem)

	if err != nil {
		p := &g
		p.BuildExecutionEnvironment(false, err)
		RecordConcurrentError(p.Nonmem.Model, "An error occurred during the creation of the executable script for this model", err, channels, p.Cancel, p)

		return
	}

	//rwxr-x---
	err = afero.WriteFile(fs, filepath.Join(g.Nonmem.OutputDir, "grid.sh"), scriptContents, 0750)

	if viper.GetBool("debug") {
		lines, _ := utils.ReadLines(filepath.Join(g.Nonmem.OutputDir, "grid.sh"))
		log.Debugf("%s %s submission script content is \n%s",
			g.Nonmem.LogIdentifier(), g.Spec.Name, strings.Join(lines, "\n"))
	}

	if err != nil {
		p := &g
		p.BuildExecutionEnvironment(false, err)
		RecordConcurrentError(p.Nonmem.Model, "There was an issue writing the executable file", err, channels, p.Cancel, p)
	}
}

// Work describes the Turnstile execution phase -> IE What heavy lifting should be done.
func (g gridModel) Work(channels *turnstile.ChannelMap) {
	cerr := executeGridJob(&g)

	if cerr.Error != nil {
		p := &g
		p.BuildExecutionEnvironment(false, cerr.Error)
		RecordConcurrentError(p.Nonmem.Model, cerr.Notes, cerr.Error, channels, p.Cancel, p)

		return
	}

	log.Debugf("%s Work is completed. Updating turnstile channels", g.Nonmem.LogIdentifier())
	log.Debugf("%s No monitor or cleanup phases currently exist for %s Job. Work completed",
		g.Nonmem.LogIdentifier(), g.Spec.Name)
	channels.Completed <- 1
}

// Monitor is the 3rd phase of turnstile (not implemented here).
func (g gridModel) Monitor(_ *turnstile.ChannelMap) {
	//Do nothing for this implementation
}

// Cleanup is the last phase of execution, in which computation / hard work is done and we're cleaning up leftover files, copying results around et all.
func (g gridModel) Cleanup(_ *turnstile.ChannelMap) {
	//err := configlib.WriteViperConfig(l.Nonmem.OutputDir, true)
	//
	//if err != nil {
	//	log.Errorf("Errors were experienced while trying to update the config at %s. Error is: %s", filepath.Join(l.Nonmem.OriginalPath, "bbi.yaml"), err)
	//}
}

//End Scalable method definitions

func (s *gridSpec) run(args []string) {
	config, err := configlib.LocateAndReadConfigFile()
	if err != nil {
		log.Fatalf("Failed to process configuration: %s", err)
	}

	log.Info("Beginning Local Path")

	logSetup(config)

	log.Debug("Searching for models based on arguments")
	models, err := s.modelsFromArguments(args, config)
	if err != nil {
		log.Fatalf("An error occurred during model processing: %s", err)
	}

	if len(models) == 0 {
		log.Fatal("No models were located or loaded. Please verify the arguments provided and try again")
	}

	//Models Added
	log.Infof("A total of %d models have been located for work", len(models))

	//Create signature safe slice for manager
	var scalables []turnstile.Scalable

	for _, v := range models {
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
	if len(models) > 0 && viper.ConfigFileUsed() != "" {
		configlib.SaveConfig(models[0].Nonmem.OriginalPath)
	}

	postWorkNotice(m, now)

	if len(m.ErrorList) > 0 {
		os.Exit(1)
	}
}

func executeGridJob(g *gridModel) turnstile.ConcurrentError {
	model := g.Nonmem
	log.Printf("%s Beginning %s work phase", model.LogIdentifier(), g.Spec.Name)
	//Execute the script we created

	scriptName := "grid.sh"

	binary, err := exec.LookPath(g.Spec.SubmitCommand)
	if err != nil {
		return turnstile.ConcurrentError{
			RunIdentifier: model.Model,
			Notes:         fmt.Sprintf("could not locate %s in path", g.Spec.SubmitCommand),
			Error:         err,
		}
	}

	command := exec.Command(binary, filepath.Join(model.OutputDir, scriptName))

	//Print the whole command if we're in debug mode
	log.Debugf("%s command is %s", g.Spec.Name, command.String())

	command.Env = os.Environ() //Take in OS Environment

	additionalEnvs := model.Configuration.GetPostWorkExecEnvs()

	if len(additionalEnvs) > 0 {
		log.Debugf("Additional post work envs were provided. Total of %d", len(additionalEnvs))
		command.Env = append(command.Env, additionalEnvs...)
	}

	return runModelCommand(model, command, g.Spec.IgnoreError)
}

func (s *gridSpec) modelsFromArguments(args []string, config configlib.Config) ([]gridModel, error) {
	var output []gridModel
	nonmemmodels, err := nonmemModelsFromArguments(args, config)

	if err != nil {
		return output, err
	}

	for _, v := range nonmemmodels {
		n := v
		output = append(output, gridModel{
			Spec:   s,
			Nonmem: &n,
			Cancel: turnstile.CancellationChannel(),
		})
	}

	return output, nil
}

// Generate the command line script to execute bbi on the grid.
func generateBbiScript(fileTemplate string, l NonMemModel) ([]byte, error) {
	t, err := utils.NewScriptTemplate(fileTemplate)
	if err != nil {
		return []byte{}, fmt.Errorf("parsing bbi script template failed: %w", err)
	}

	buf := new(bytes.Buffer)

	jobname, err := gridengineJobName(&l)
	if err != nil {
		return nil, fmt.Errorf("failed to generate job name: %w", err)
	}

	filename := l.Model

	if l.Configuration.NMQual {
		filename = l.FileName + ".ctl"
	}

	type content struct {
		Command          []string
		Config           configlib.Config
		JobName          string
		WorkingDirectory string
	}

	binary := l.Configuration.BbiBinary
	if binary == "" {
		binary, err = os.Executable()
		if err != nil {
			return nil, fmt.Errorf("unable to get path to executed binary: %w", err)
		}
	}
	commandComponents := []string{
		binary,
		"nonmem",
		"run",
		"local",
		filename,
	}

	if !l.Configuration.Local.CreateChildDirs {
		commandComponents = append(commandComponents, "--create_child_dirs=false")
	}

	log.Debugf("command: %v", commandComponents)

	err = t.Execute(buf, content{
		Command:          commandComponents,
		Config:           l.Configuration,
		JobName:          jobname,
		WorkingDirectory: l.OutputDir,
	})

	if err != nil {
		return []byte{}, fmt.Errorf("failed to generate bbi script: %w", err)
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
