package cmd

import (
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"os"
	"os/exec"
	"path"
	"strconv"
)

const runLongDescription string = `run nonmem model(s), for example: 
bbi nonmem run <local|sge> run001.mod
bbi nonmem run  --clean_lvl=1 <local|sge> run001.mod run002.mod
bbi nonmem run <local|sge> run[001:006].mod // expand to run001.mod run002.mod ... run006.mod local
bbi nonmem run <local|sge> .// run all models in directory
 `

// RunCmd represents the run command
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

	//String Variables
	// runCmd.PersistentFlags().String("cacheDir", "", "directory path for cache of nonmem executables for NM7.4+")
	// viper.BindPFlag("cacheDir", runCmd.PersistentFlags().Lookup("cacheDir"))

	// runCmd.PersistentFlags().String("cacheExe", "", "name of executable stored in cache")
	// viper.BindPFlag("cacheExe", runCmd.PersistentFlags().Lookup("cacheExe"))

	// runCmd.PersistentFlags().String("saveExe", "", "what to name the executable when stored in cache")
	// viper.BindPFlag("saveExe", runCmd.PersistentFlags().Lookup("saveExe"))

	runCmd.PersistentFlags().String("output_dir", "{{ .Name }}", "Go template for the output directory to use for storging details of each executed model")
	viper.BindPFlag("output_dir", runCmd.PersistentFlags().Lookup("output_dir"))
	viper.SetDefault("output_dir", "{{ .Name }}")

	//Int Variables
	runCmd.PersistentFlags().Int("clean_lvl", 1, "clean level used for file output from a given (set of) runs")
	viper.BindPFlag("clean_lvl", runCmd.PersistentFlags().Lookup("clean_lvl"))
	// TODO: these are likely not meangingful as should be set in configlib, but want to configm
	viper.SetDefault("clean_lvl", 1)

	runCmd.PersistentFlags().Int("copy_lvl", 0, "copy level used for file output from a given (set of) runs")
	viper.BindPFlag("copy_lvl", runCmd.PersistentFlags().Lookup("copy_lvl"))
	viper.SetDefault("copy_lvl", 0)

	// runCmd.PersistentFlags().Int("gitignoreLvl", 0, "gitignore lvl for a given (set of) runs")
	// viper.BindPFlag("gitignoreLvl", runCmd.PersistentFlags().Lookup("gitignoreLvl"))
	// viper.SetDefault("gitignoreLvl", 1)

	//Bool Variables
	runCmd.PersistentFlags().Bool("git", false, "whether git is used")
	viper.BindPFlag("git", runCmd.PersistentFlags().Lookup("git"))
	viper.SetDefault("git", true)

	runCmd.PersistentFlags().Bool("overwrite", false, "Whether or not to remove existing output directories if they are present")
	viper.BindPFlag("overwrite", runCmd.PersistentFlags().Lookup("overwrite"))
	viper.SetDefault("overwrite", false)

	const configIdentifier string = "config"
	runCmd.PersistentFlags().String(configIdentifier, "", "Path (relative or absolute) to another babylon.yaml to load")
	viper.BindPFlag(configIdentifier, runCmd.PersistentFlags().Lookup(configIdentifier))

	const saveconfig string = "save_config"
	runCmd.PersistentFlags().Bool(saveconfig, true, "Whether or not to save the existing configuration to a file with the model")
	viper.BindPFlag(saveconfig, runCmd.PersistentFlags().Lookup(saveconfig))

	const delayIdentifier string = "delay"
	runCmd.PersistentFlags().Int(delayIdentifier, 0, "Selects a random number of seconds between 1 and this value to stagger / jitter job execution. Assists in dealing with large volumes of work dealing with the same data set. May avoid NMTRAN issues about not being able read / close files")
	viper.BindPFlag(delayIdentifier, runCmd.PersistentFlags().Lookup(delayIdentifier))

	const logFileIdentifier string = "log_file"
	runCmd.PersistentFlags().String(logFileIdentifier, "", "If populated, specifies the file into which to store the output / logging details from Babylon")
	viper.BindPFlag(logFileIdentifier, runCmd.PersistentFlags().Lookup(logFileIdentifier))

	const postExecutionHookIdentifier string = "post_work_executable"
	runCmd.PersistentFlags().String(postExecutionHookIdentifier, "", "A script or binary to run when job execution completes or fails")
	viper.BindPFlag(postExecutionHookIdentifier, runCmd.PersistentFlags().Lookup(postExecutionHookIdentifier))

	nonmemCmd.AddCommand(runCmd)

}

type PostExecutionHookEnvironment struct {
	ExecutionBinary string `yaml:"execution_binary" json:"execution_binary, omitempty"`
	ModelPath       string `yaml:"model_path" json:"model_path,omitempty"`
	Model           string `yaml:"model" json:"model,omitempty"`
	Filename        string `yaml:"filename" json:"filename,omitempty"`
	Extension       string `yaml:"extension" json:"extension,omitempty"`
	OutputDirectory string `yaml:"output_directory" json:"output_directory,omitempty"`
	Successful      bool   `yaml:"successful" json:"successful,omitempty"`
	Error           error  `yaml:"error" json:"error,omitempty"`
}

//NewPostHookEnvironmentFromNonMemModel creates an execution directive from the common model. Will eventually have
//another method for STAN or other execution types
func NewPostHookEnvironmentFromNonMemModel(execution_binary string, nonmem *NonMemModel, successful bool, err error) PostExecutionHookEnvironment {
	return PostExecutionHookEnvironment{
		ModelPath:       nonmem.OutputDir,
		Model:           nonmem.Model,
		Filename:        nonmem.FileName,
		Extension:       nonmem.Extension,
		OutputDirectory: nonmem.OutputDir,
		Successful:      successful,
		Error:           err,
		ExecutionBinary: execution_binary,
	}
}

func PostExecutionHook(directive PostExecutionHookEnvironment) (string, error) {
	environmentalPrefix := "BABYLON_"
	var fullyQualifiedExecutionPath string

	//Is the path provided for execution Binary absolute?
	if path.IsAbs(directive.ExecutionBinary) {
		//Nothing to change. We'll use this value
		fullyQualifiedExecutionPath = directive.ExecutionBinary
	} else {
		//Need to get current dir and append to value
		whereami, err := os.Getwd()
		if err != nil {
			return "", err
		}

		fullyQualifiedExecutionPath = path.Join(whereami, directive.ExecutionBinary)
	}

	pathEnvironment := environmentalPrefix + "MODEL_PATH"
	modelEnvironment := environmentalPrefix + "MODEL"
	filenameEnvironment := environmentalPrefix + "FILENAME"
	extensionEnvironment := environmentalPrefix + "MODEL_EXT"
	outputDirEnvironment := environmentalPrefix + "OUTPUT_DIR"
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

	executionEnvironment = append(executionEnvironment, errorEnvironment+"="+errorText)

	cmd := exec.Command(fullyQualifiedExecutionPath)

	//Set the environment for the binary.
	cmd.Env = executionEnvironment
	bytes, err := cmd.CombinedOutput()

	return string(bytes), err
}
