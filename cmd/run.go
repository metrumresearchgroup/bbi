package cmd

import (
	"bytes"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"text/template"

	"github.com/metrumresearchgroup/bbi/configlib"

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

func run(_ *cobra.Command, _ []string) {
	println(runLongDescription)
}

func NewRunCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "run",
		Short: "run a (set of) models locally or on the grid",
		Long:  runLongDescription,
		Run:   run,
	}

	// String Variables
	// cmd.PersistentFlags().String("cacheDir", "", "directory path for cache of nonmem executables for NM7.4+")
	// viper.BindPFlag("cacheDir", cmd.PersistentFlags().Lookup("cacheDir"))

	// cmd.PersistentFlags().String("cacheExe", "", "name of executable stored in cache")
	// viper.BindPFlag("cacheExe", cmd.PersistentFlags().Lookup("cacheExe"))

	// cmd.PersistentFlags().String("saveExe", "", "what to name the executable when stored in cache")
	// viper.BindPFlag("saveExe", cmd.PersistentFlags().Lookup("saveExe"))

	cmd.PersistentFlags().String("output_dir", "{{ .Name }}", "Go template for the output directory to use for storing details of each executed model")
	errpanic(viper.BindPFlag("output_dir", cmd.PersistentFlags().Lookup("output_dir")))
	viper.SetDefault("output_dir", "{{ .Name }}")

	// Int Variables
	cmd.PersistentFlags().Int("clean_lvl", 1, "clean level used for output")
	errpanic(viper.BindPFlag("clean_lvl", cmd.PersistentFlags().Lookup("clean_lvl")))
	// TODO: these are likely not meangingful as should be set in configlib, but want to configm
	viper.SetDefault("clean_lvl", 1)

	cmd.PersistentFlags().Int("copy_lvl", 0, "copy level used for output")
	errpanic(viper.BindPFlag("copy_lvl", cmd.PersistentFlags().Lookup("copy_lvl")))
	viper.SetDefault("copy_lvl", 0)

	// cmd.PersistentFlags().Int("gitignoreLvl", 0, "gitignore lvl for a given (set of) runs")
	// viper.BindPFlag("gitignoreLvl", cmd.PersistentFlags().Lookup("gitignoreLvl"))
	// viper.SetDefault("gitignoreLvl", 1)

	// Bool Variables
	cmd.PersistentFlags().Bool("git", false, "whether git is used")
	errpanic(viper.BindPFlag("git", cmd.PersistentFlags().Lookup("git")))
	viper.SetDefault("git", true)

	cmd.PersistentFlags().Bool("overwrite", false, "whether to remove existing output directories")
	errpanic(viper.BindPFlag("overwrite", cmd.PersistentFlags().Lookup("overwrite")))
	viper.SetDefault("overwrite", false)

	const configIdentifier string = "config"
	cmd.PersistentFlags().String(configIdentifier, "", "path to another bbi.yaml to load")
	errpanic(viper.BindPFlag(configIdentifier, cmd.PersistentFlags().Lookup(configIdentifier)))

	const saveconfig string = "save_config"
	cmd.PersistentFlags().Bool(saveconfig, true, "whether to save the existing configuration to the output directory")
	errpanic(viper.BindPFlag(saveconfig, cmd.PersistentFlags().Lookup(saveconfig)))

	const delayIdentifier string = "delay"
	cmd.PersistentFlags().Int(delayIdentifier, 0, "Selects a random number of seconds between 1 and this value to stagger / jitter job execution. Assists in dealing with large volumes of work dealing with the same data set. May avoid NMTRAN issues about not being able read / close files")
	errpanic(viper.BindPFlag(delayIdentifier, cmd.PersistentFlags().Lookup(delayIdentifier)))

	const logFileIdentifier string = "log_file"
	cmd.PersistentFlags().String(logFileIdentifier, "", "file into which to store the output / logging details from bbi")
	errpanic(viper.BindPFlag(logFileIdentifier, cmd.PersistentFlags().Lookup(logFileIdentifier)))

	const postExecutionHookIdentifier string = "post_work_executable"
	cmd.PersistentFlags().String(postExecutionHookIdentifier, "", "script or binary to run when job execution completes or fails")
	errpanic(viper.BindPFlag(postExecutionHookIdentifier, cmd.PersistentFlags().Lookup(postExecutionHookIdentifier)))

	const additionalEnvIdentifier string = "additional_post_work_envs"
	cmd.PersistentFlags().StringSlice(additionalEnvIdentifier, []string{}, "additional values (as ENV KEY=VALUE) to provide for the post execution environment")
	errpanic(viper.BindPFlag(additionalEnvIdentifier, cmd.PersistentFlags().Lookup(additionalEnvIdentifier)))

	cmd.AddCommand(NewLocalCmd())
	cmd.AddCommand(NewSgeCmd())

	return cmd
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

func PostWorkExecution(job PostWorkExecutor, successful bool, err error) {
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

	// TODO: use more secure permission of 0600 or less
	err = os.WriteFile(filepath.Join(worker.GetWorkingPath(), "post_processing.sh"), processedBytes, 0755) // nolint:gosec

	if err != nil {
		return "", err
	}

	bash, err := exec.LookPath("bash")
	if err != nil {
		return "", err
	}

	// Needs to be the processed value, not the config template.
	cmd := exec.Command(bash, filepath.Join(worker.GetWorkingPath(), "post_processing.sh"))

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
