package cmd

import (
	"bytes"
	"errors"
	"fmt"
	"log"
	"os/exec"
	"path"
	"regexp"
	"strings"
	"text/template"
	"time"

	"os"

	"github.com/metrumresearchgroup/babylon/runner"
	"github.com/metrumresearchgroup/babylon/utils"
	"github.com/metrumresearchgroup/turnstile"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var arguments []string

type localOperation struct {
	Models []localModel `json:"models"`
}

type localModel struct {
	//ModelName is the name of the model on which we will action: acop.mod
	ModelName string `json:"model_name"`
	//ModelPath is the Fully Qualified Path to the original model
	ModelPath string `json:"model_path"`
	//ModelFileName is the Filename component (sans extension)
	ModelFileName string `json:"model_filename"`
	//ModelExtension is the extension of the file
	ModelExtension string `json:"model_extension"`
	//OriginalPath is the path at which the original model was located: /Users/Documents/acop/
	OriginalPath string `json:"original_path"`
	//OutputDir is the directory into which the copied models and work will be located
	OutputDir string `json:"output_dir"`
	//Settings are basically the cobra definitions / requirements for the iteration
	Settings runner.RunSettings `json:"settings"`
	//Whether or not the model had an error on generation or execution
	Error error `json:"error"`
}

func newLocalModel(modelname string) localModel {

	fs := afero.NewOsFs()
	cwd, err := os.Getwd()
	lm := localModel{}

	if err != nil {
		return localModel{
			Error: errors.New("Unable to locate the current working directory: " + err.Error()),
		}
	}

	//Is this path relative? Try combining the provided value with current dir
	joined := path.Join(cwd, modelname)

	if ok, _ := afero.Exists(fs, joined); ok {
		//This is a relative path.
		lm.ModelPath = joined
	}

	if ok, _ := afero.Exists(fs, modelname); ok {
		//Arg is an absolut epath
		lm.ModelPath = modelname
	}

	fi, err := fs.Stat(lm.ModelPath)

	if err != nil {
		return localModel{
			Error: err,
		}
	}

	lm.ModelName = fi.Name()

	modelPieces := strings.Split(lm.ModelName, ".")

	lm.ModelFileName = modelPieces[0]

	//Don't assume file will have extension
	if len(modelPieces) > 1 {
		lm.ModelExtension = modelPieces[1]
	}

	//Get the raw path of the original by stripping the actual file from it
	lm.OriginalPath = strings.Replace(lm.ModelPath, "/"+lm.ModelName, "", 1)

	//Process The template from the viper content for output Dir
	t, err := template.New("output").Parse(outputDir)
	buf := new(bytes.Buffer)

	if err != nil {
		return localModel{
			Error: errors.New("There was an issue parsing the template provided: " + err.Error()),
		}
	}

	type outputName struct {
		Name string
	}

	//Make sure to only use the filename for the output dir
	err = t.Execute(buf, outputName{
		Name: lm.ModelFileName,
	})

	if err != nil {
		return localModel{
			Error: err,
		}
	}

	//Use the template content plus the original path
	lm.OutputDir = path.Join(lm.OriginalPath, buf.String())

	if err != nil {
		return localModel{
			Error: errors.New("There was an issue executing the provided template: " + err.Error()),
		}
	}

	lm.Settings = runner.RunSettings{
		Git:                viper.GetBool("git"),
		SaveExe:            saveExe,
		Verbose:            verbose,
		Debug:              debug,
		CleanLvl:           viper.GetInt("cleanLvl"),
		CopyLvl:            viper.GetInt("copyLvl"),
		CacheDir:           viper.GetString("cacheDir"),
		ExeNameInCache:     viper.GetString("cacheExe"),
		NmExecutableOrPath: viper.GetString("nmExecutable"),
		OneEst:             viper.GetBool("oneEst"),
		Overwrite:          viper.GetBool("overwrite"),
	}

	return lm
}

//Begin Scalable method definitions

//Prepare is basically the old EstimateModel function. Responsible for creating directories and preparation.
func (l localModel) Prepare(channels *turnstile.ChannelMap) {
	//Mark the model as started some work
	channels.Working <- 1
	fs := afero.NewOsFs()

	//Does output directory exist?
	if ok, _ := afero.Exists(fs, l.OutputDir); ok {
		//If so are we configured to overwrite?
		if l.Settings.Overwrite {
			err := fs.RemoveAll(l.OutputDir)
			if err != nil {
				channels.Failed <- 1
				channels.Errors <- turnstile.ConcurrentError{
					RunIdentifier: l.ModelName,
					Error:         err,
					Notes:         "An error occured trying to remove the directory as specified in the overwrite flag",
				}
				return
			}
		} else {
			channels.Failed <- 1
			channels.Errors <- turnstile.ConcurrentError{
				RunIdentifier: l.ModelName,
				Error:         errors.New("The output directory already exists"),
				Notes:         fmt.Sprintf("The target directory, %s already, exists, but we are configured to not overwrite. Invalid configuration / run state", l.OutputDir),
			}
			return
		}
	}

	//Copy Model into destination and update Data Path
	err := copyFileToDestination(l, true)

	if err != nil {
		channels.Failed <- 1
		channels.Errors <- turnstile.ConcurrentError{
			RunIdentifier: l.ModelName,
			Error:         err,
			Notes:         fmt.Sprintf("There appears to have been an issue trying to copy %s to %s", l.ModelName, l.OutputDir),
		}
		return
	}

	//Create Execution Script
	scriptContents, err := generateScript(scriptTemplate, l)

	if err != nil {
		channels.Failed <- 1
		channels.Errors <- turnstile.ConcurrentError{
			RunIdentifier: l.ModelName,
			Error:         err,
			Notes:         "An error occurred during the creation of the executable script for this model",
		}
	}

	//rwxr-x---
	afero.WriteFile(fs, path.Join(l.OutputDir, l.ModelFileName+".sh"), scriptContents, 0750)
}

func (l localModel) Work(channels *turnstile.ChannelMap) {
	log.Printf("Beginning work phase for %s", l.ModelFileName)
	fs := afero.NewOsFs()
	//Execute the script we created
	os.Chdir(l.OutputDir)
	scriptLocation := "./" + l.ModelFileName + ".sh"

	command := exec.Command(scriptLocation)
	command.Env = os.Environ() //Take in OS Environment

	output, err := command.CombinedOutput()

	if err != nil {
		channels.Failed <- 1
		channels.Errors <- turnstile.ConcurrentError{
			RunIdentifier: l.ModelFileName,
			Error:         err,
			Notes:         "Running the programmatic shell script caused an error",
		}
		return
	}

	afero.WriteFile(fs, path.Join(l.OutputDir, l.ModelName+".out"), output, 0750)

	//Mark as completed and move on to cleanup
	channels.Completed <- 1

}

func (l localModel) Monitor(channels *turnstile.ChannelMap) {
	//Do nothing for this implementation
}

func (l localModel) Cleanup(channels *turnstile.ChannelMap) {
	//Copy Up

	//Clean Up
}

//End Scalable method definitions

// runCmd represents the run command
var localCmd = &cobra.Command{
	Use:   "local",
	Short: "local specifies to run a (set of) models locally",
	Long: `run model(s), for example: 
bbi nonmem local run001.mod
bbi nonmem --cleanLvl=1 local run001.mod run002.mod 
bbi nonmem local run[001:006].mod // expand to run001.mod run002.mod ... run006.mod 
bbi nonmem local . // run all models in directory
 `,
	Run: local,
}

func init() {
	RootCmd.AddCommand(nonmemCmd)
	localCmd.Flags().StringVar(&cacheDir, "cacheDir", "", "directory path for cache of nonmem executables for NM7.4+")
	localCmd.Flags().StringVar(&cacheExe, "cacheExe", "", "name of executable stored in cache")
	localCmd.Flags().StringVar(&saveExe, "saveExe", "", "what to name the executable when stored in cache")
	localCmd.Flags().IntVar(&cleanLvl, "cleanLvl", 0, "clean level used for file output from a given (set of) runs")
	localCmd.Flags().IntVar(&copyLvl, "copyLvl", 0, "copy level used for file output from a given (set of) runs")
	localCmd.Flags().IntVar(&gitignoreLvl, "gitignoreLvl", 0, "gitignore lvl for a given (set of) runs")
	localCmd.Flags().BoolVar(&git, "git", false, "whether git is used")
	localCmd.Flags().StringVar(&outputDir, "outputDir", "{{ .Name }}", "Go template for the output directory to use for storging details of each executed model")
	localCmd.Flags().BoolVar(&overwrite, "overwrite", true, "Whether or not to remove existing output directories if they are present")
}

func local(cmd *cobra.Command, args []string) {

	if flagChanged(cmd.Flags(), "cacheDir") {
		viper.Set("cacheDir", cacheDir)
	}
	if flagChanged(cmd.Flags(), "cacheExe") {
		fmt.Println("setting new cache exe of: ", cacheExe)
		viper.Set("cacheExe", cacheExe)
	}
	if flagChanged(cmd.Flags(), "cleanLvl") {
		viper.Set("cleanLvl", cleanLvl)
	}
	if flagChanged(cmd.Flags(), "copyLvl") {
		viper.Set("copyLvl", copyLvl)
	}
	if flagChanged(cmd.Flags(), "git") {
		viper.Set("git", git)
	}
	if flagChanged(cmd.Flags(), "threads") {
		viper.Set("threads", threads)
	}
	if flagChanged(cmd.Flags(), "outputDir") {
		viper.Set("outputDir", outputDir)
	}
	if flagChanged(cmd.Flags(), "overwrite") {
		viper.Set("overwrite", overwrite)
	}

	if debug {
		viper.Debug()
	}

	AppFs := afero.NewOsFs()
	lo := localOperation{}

	if verbose {
		log.Printf("setting up a work queue with %v workers", viper.GetInt("threads"))
	}

	// regex for filename expansion check
	r := regexp.MustCompile("(.*)?\\[(.*)\\](.*)?")

	//Let's process our args into models
	for _, arg := range args {

		// check if arg is a file or Dir
		// dirty check for if doesn't have an extension is a folder
		_, ext := utils.FileAndExt(arg)
		if ext == "" || arg == "." {
			// could be directory, will need to be careful about the waitgroup as don't want to
			// keep waiting forever since it
			isDir, err := utils.IsDir(arg, AppFs)
			if err != nil || !isDir {
				log.Printf("issue handling %s, if this is a run please add the extension. Err: (%s)", arg, err)
				continue
			}
			modelsInDir, err := utils.ListModels(arg, ".mod", AppFs)
			if err != nil {
				log.Printf("issue getting models in dir %s, if this is a run please add the extension. Err: (%s)", arg, err)
				continue
			}
			if verbose || debug {
				log.Printf("adding %v model files in directory %s to queue", len(modelsInDir), arg)
			}

			for _, model := range modelsInDir {
				lo.Models = append(lo.Models, newLocalModel(model))
			}

		} else {
			// figure out if need to do expansion, or run as-is
			if len(r.FindAllStringSubmatch(arg, 1)) > 0 {
				log.Printf("expanding model pattern: %s \n", arg)
				pat, err := utils.ExpandNameSequence(arg)
				if err != nil {
					log.Printf("err expanding name: %v", err)
					// don't try to run this model
					continue
				}
				if verbose || debug {
					log.Printf("expanded models: %s \n", pat)
				}
				for _, p := range pat {
					lo.Models = append(lo.Models, newLocalModel(p))
				}
			} else {
				lo.Models = append(lo.Models, newLocalModel(arg))
			}
		}
	}

	if len(lo.Models) == 0 {
		log.Fatal("No models were located or loaded. Please verify the arguments provided and try again")
	}

	//Display Summary

	//Models Added
	log.Printf("A total of %d models have been located for work", len(lo.Models))

	//Models in Error
	//Locate 'em
	counter := 0
	var errors []localModel
	for _, v := range lo.Models {
		if v.Error != nil {
			counter++
			errors = append(errors, v)
		}
	}

	if counter > 0 {
		log.Printf("It appears that %d models generated an error during the initial setup phase", len(errors))
		for _, v := range errors {
			log.Printf("Model named %s has errored. Details: %s", v.ModelName, v.Error.Error())
		}
	}

	//Models in OK state
	log.Printf("%d models successfully completed initial setup phase.", len(lo.Models)-len(errors))

	//Create signature safe slice for manager
	var scalables []turnstile.Scalable

	for _, v := range lo.Models {
		scalables = append(scalables, v)
	}

	//Begin Execution
	m := turnstile.NewManager(scalables, uint64(viper.GetInt("threads")))

	now := time.Now()

	go m.Execute()

	//Now we're doing the work. Is we done?

	//Basically wait
	for !m.IsComplete() {
		time.Sleep(5 * time.Millisecond)
	}

	fmt.Printf("\r%d models completed in %s", m.Completed, time.Since(now))
	println("")
}

func init() {
	nonmemCmd.AddCommand(localCmd)
}
