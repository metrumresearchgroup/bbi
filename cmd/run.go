package cmd

import (
	"bytes"
	"errors"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"text/template"

	"bbi/configlib"

	"github.com/metrumresearchgroup/turnstile"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const runLongDescription string = `run nonmem model(s), for example: 
bbi nonmem run <local|sge> run001.mod
bbi nonmem run  --clean_lvl=1 <local|sge> run001.mod run002.mod
bbi nonmem run <local|sge> run[001:006].mod // expand to run001.mod run002.mod ... run006.mod local
bbi nonmem run <local|sge> .// run all models in directory
 `

const postProcessingScriptTemplate string = `#!/bin/bash

###################
#
# bbi post-processing script
# 
# Please note that the environment variables written here
# do not represent all used during execution. Private variables,
# such as additional environments provided for external systems authentication
# are not written into this file for security reasons.
#
# Only environment values prefixed with BBI_ are written into this file at
# execution time.
#
###################

{{ range .EnvironmentVariables }}
	export {{ . }}
{{- end}}


###################
#
# Below is the actual post execution target.
#
###################
{{ .Script }}
`

// RunCmd represents the run command.
var runCmd = &cobra.Command{
	Use:   "run",
	Short: "run a (set of) models locally or on the grid",
	Long:  runLongDescription,
	Run:   run,
}

func run(cmd *cobra.Command, args []string) {
	println(runLongDescription)
}

func init() {
	// String Variables
	// runCmd.PersistentFlags().String("cacheDir", "", "directory path for cache of nonmem executables for NM7.4+")
	// viper.BindPFlag("cacheDir", runCmd.PersistentFlags().Lookup("cacheDir"))

	// runCmd.PersistentFlags().String("cacheExe", "", "name of executable stored in cache")
	// viper.BindPFlag("cacheExe", runCmd.PersistentFlags().Lookup("cacheExe"))

	// runCmd.PersistentFlags().String("saveExe", "", "what to name the executable when stored in cache")
	// viper.BindPFlag("saveExe", runCmd.PersistentFlags().Lookup("saveExe"))

	runCmd.PersistentFlags().String("output_dir", "{{ .Name }}", "Go template for the output directory to use for storging details of each executed model")
	errpanic(viper.BindPFlag("output_dir", runCmd.PersistentFlags().Lookup("output_dir")))
	viper.SetDefault("output_dir", "{{ .Name }}")

	// Int Variables
	runCmd.PersistentFlags().Int("clean_lvl", 1, "clean level used for file output from a given (set of) runs")
	errpanic(viper.BindPFlag("clean_lvl", runCmd.PersistentFlags().Lookup("clean_lvl")))
	// TODO: these are likely not meangingful as should be set in configlib, but want to configm
	viper.SetDefault("clean_lvl", 1)

	runCmd.PersistentFlags().Int("copy_lvl", 0, "copy level used for file output from a given (set of) runs")
	errpanic(viper.BindPFlag("copy_lvl", runCmd.PersistentFlags().Lookup("copy_lvl")))
	viper.SetDefault("copy_lvl", 0)

	// runCmd.PersistentFlags().Int("gitignoreLvl", 0, "gitignore lvl for a given (set of) runs")
	// viper.BindPFlag("gitignoreLvl", runCmd.PersistentFlags().Lookup("gitignoreLvl"))
	// viper.SetDefault("gitignoreLvl", 1)

	// Bool Variables
	runCmd.PersistentFlags().Bool("git", false, "whether git is used")
	errpanic(viper.BindPFlag("git", runCmd.PersistentFlags().Lookup("git")))
	viper.SetDefault("git", true)

	runCmd.PersistentFlags().Bool("overwrite", false, "Whether or not to remove existing output directories if they are present")
	errpanic(viper.BindPFlag("overwrite", runCmd.PersistentFlags().Lookup("overwrite")))
	viper.SetDefault("overwrite", false)

	const configIdentifier string = "config"
	runCmd.PersistentFlags().String(configIdentifier, "", "Path (relative or absolute) to another bbi.yaml to load")
	errpanic(viper.BindPFlag(configIdentifier, runCmd.PersistentFlags().Lookup(configIdentifier)))

	const saveconfig string = "save_config"
	runCmd.PersistentFlags().Bool(saveconfig, true, "Whether or not to save the existing configuration to a file with the model")
	errpanic(viper.BindPFlag(saveconfig, runCmd.PersistentFlags().Lookup(saveconfig)))

	const delayIdentifier string = "delay"
	runCmd.PersistentFlags().Int(delayIdentifier, 0, "Selects a random number of seconds between 1 and this value to stagger / jitter job execution. Assists in dealing with large volumes of work dealing with the same data set. May avoid NMTRAN issues about not being able read / close files")
	errpanic(viper.BindPFlag(delayIdentifier, runCmd.PersistentFlags().Lookup(delayIdentifier)))

	const logFileIdentifier string = "log_file"
	runCmd.PersistentFlags().String(logFileIdentifier, "", "If populated, specifies the file into which to store the output / logging details from bbi")
	errpanic(viper.BindPFlag(logFileIdentifier, runCmd.PersistentFlags().Lookup(logFileIdentifier)))

	const postExecutionHookIdentifier string = "post_work_executable"
	runCmd.PersistentFlags().String(postExecutionHookIdentifier, "", "A script or binary to run when job execution completes or fails")
	errpanic(viper.BindPFlag(postExecutionHookIdentifier, runCmd.PersistentFlags().Lookup(postExecutionHookIdentifier)))

	const additionalEnvIdentifier string = "additional_post_work_envs"
	runCmd.PersistentFlags().StringSlice(additionalEnvIdentifier, []string{}, "Any additional values (as ENV KEY=VALUE) to provide for the post execution environment")
	errpanic(viper.BindPFlag(additionalEnvIdentifier, runCmd.PersistentFlags().Lookup(additionalEnvIdentifier)))

	nonmemCmd.AddCommand(runCmd)
}

type PostWorkExecutor interface {
	BuildExecutionEnvironment(completed bool, err error) // Sets the Struct content for the PostExecutionHookEnvironment
	GetPostWorkConfig() *PostExecutionHookEnvironment
	GetPostWorkExecutablePath() string
	GetGlobalConfig() configlib.Config
	GetWorkingPath() string
}

type PostExecutionHookEnvironment struct {
	ExecutionBinary string `yaml:"execution_binary" json:"execution_binary,omitempty"`
	ModelPath       string `yaml:"model_path" json:"model_path,omitempty"`
	Model           string `yaml:"model" json:"model,omitempty"`
	Filename        string `yaml:"filename" json:"filename,omitempty"`
	Extension       string `yaml:"extension" json:"extension,omitempty"`
	OutputDirectory string `yaml:"output_directory" json:"output_directory,omitempty"`
	Successful      bool   `yaml:"successful" json:"successful,omitempty"`
	Error           error  `yaml:"error" json:"error,omitempty"`
}

func PostExecutionEnvironment(directive *PostExecutionHookEnvironment, additional []string) []string {
	environmentalPrefix := "BBI_"

	pathEnvironment := environmentalPrefix + "MODEL_PATH"
	modelEnvironment := environmentalPrefix + "MODEL"
	filenameEnvironment := environmentalPrefix + "MODEL_FILENAME"
	extensionEnvironment := environmentalPrefix + "MODEL_EXT"
	outputDirEnvironment := environmentalPrefix + "OUTPUT_DI" +
		"R"
	successEnvironment := environmentalPrefix + "SUCCESSFUL"
	errorEnvironment := environmentalPrefix + "ERROR"

	var executionEnvironment []string

	executionEnvironment = append(executionEnvironment, pathEnvironment+"="+directive.ModelPath)
	executionEnvironment = append(executionEnvironment, modelEnvironment+"="+directive.Model)
	executionEnvironment = append(executionEnvironment, filenameEnvironment+"="+directive.Filename)
	executionEnvironment = append(executionEnvironment, extensionEnvironment+"="+directive.Extension)
	executionEnvironment = append(executionEnvironment, outputDirEnvironment+"="+directive.OutputDirectory)
	executionEnvironment = append(executionEnvironment, successEnvironment+"="+strconv.FormatBool(directive.Successful))

	var errorText string

	if directive.Error != nil {
		errorText = directive.Error.Error()
	}

	executionEnvironment = append(executionEnvironment, errorEnvironment+"="+`"`+errorText+`"`)

	// Add user provided values
	executionEnvironment = append(executionEnvironment, additional...)

	return executionEnvironment
}

func PostWorkExecution(job PostWorkExecutor, _ /*filename*/ string, _ /*channels*/ *turnstile.ChannelMap, _ /*cancel*/ chan bool, successful bool, err error) {
	config := job.GetGlobalConfig()
	if config.PostWorkExecutable != "" {
		log.Debug("Beginning execution of post work hooks")
		executionWaitGroup.Add(1)
		job.BuildExecutionEnvironment(successful, err)

		type executionStatus struct {
			output string
			err    error
		}

		executionChannel := make(chan executionStatus, 1)

		go func() {
			status := <-executionChannel
			if status.err != nil {
				job.BuildExecutionEnvironment(false, status.err)
			}
			executionWaitGroup.Done()
		}()

		go func() {
			output, err := ExecutePostWorkDirectivesWithEnvironment(job)
			es := executionStatus{
				output: output,
				err:    err,
			}
			executionChannel <- es
		}()
	}
}

func ExecutePostWorkDirectivesWithEnvironment(worker PostWorkExecutor) (string, error) {
	log.Debug("Beginning Execution of post work scripts")
	var outBytesBuffer = new(bytes.Buffer)

	toExecute := worker.GetPostWorkExecutablePath()
	config := worker.GetGlobalConfig()
	postworkConfig := worker.GetPostWorkConfig()
	postWorkEnv := config.GetPostWorkExecEnvs()

	log.WithFields(log.Fields{
		"postWorkConfig": postworkConfig,
		"postWorkEnv":    postWorkEnv,
	}).Debug("Collected details. Preparing to set environment")

	environment := PostExecutionEnvironment(postworkConfig, postWorkEnv)

	environment = append(environment, os.Environ()...)

	log.WithFields(log.Fields{
		"environment": environment,
	}).Debug("Environment prepared")

	environmentToPersist := onlyBbiVariables(environment)

	log.Debug("Beginning template operations")

	tmpl, err := template.New("post_processing").Parse(postProcessingScriptTemplate)

	if err != nil {
		return "", err
	}

	type postProcessingDetails struct {
		EnvironmentVariables []string
		Script               string
	}

	ppd := postProcessingDetails{
		EnvironmentVariables: environmentToPersist,
		Script:               toExecute,
	}

	log.Debugf("Fully qualified path to executable is %s", toExecute)

	log.WithFields(log.Fields{
		"details": ppd,
	}).Debug("Executing template")

	err = tmpl.Execute(outBytesBuffer, ppd)

	if err != nil {
		return "", err
	}

	processedBytes := outBytesBuffer.Bytes()

	log.WithFields(log.Fields{
		"rendered": string(processedBytes),
	}).Debug("Writing contents to file in output directory")

	err = ioutil.WriteFile(filepath.Join(worker.GetWorkingPath(), "post_processing.sh"), processedBytes, 0755)

	if err != nil {
		return "", err
	}

	// Needs to be the processed value, not the config template.
	cmd := exec.Command(filepath.Join(worker.GetWorkingPath(), "post_processing.sh"))

	// Set the environment for the binary.
	cmd.Env = environment

	log.WithFields(log.Fields{
		"environment": cmd.Env,
	}).Debug("Environment generated for commandline")

	log.Debugf("Command will be %s", cmd.String())

	outputBytes, err := cmd.CombinedOutput()

	log.Debugf("Output from command was %s", string(outputBytes))

	if err != nil {
		var exitError *exec.ExitError
		if errors.As(err, &exitError) {
			code := exitError.ExitCode()
			details := exitError.String()

			log.Errorf("Exit code was %d, details were %s", code, details)
			log.Errorf("output details were: %s", string(outputBytes))
		}
	}

	return string(outputBytes), err
}

func onlyBbiVariables(provided []string) []string {
	var matched []string
	for _, v := range provided {
		if strings.HasPrefix(v, "BBI_") {
			matched = append(matched, v)
		}
	}

	return matched
}
