package cmd

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"os/exec"
	"path"
	"path/filepath"
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
	Models []NonMemModel `json:"models"`
}

//NonMemModel is the definition of a model for NonMem including its target directories and settings required for execution
type NonMemModel struct {
	//Model is the name of the model on which we will action: acop.mod
	Model string `json:"model_name"`
	//Path is the Fully Qualified Path to the original model
	Path string `json:"model_path"`
	//FileName is the Filename component (sans extension)
	FileName string `json:"model_filename"`
	//Extension is the extension of the file
	Extension string `json:"model_extension"`
	//OriginalPath is the path at which the original model was located: /Users/Documents/acop/
	OriginalPath string `json:"original_path"`
	//OutputDir is the directory into which the copied models and work will be located
	OutputDir string `json:"output_dir"`
	//Settings are basically the cobra definitions / requirements for the iteration
	Settings runner.RunSettings `json:"settings"`
	//Whether or not the model had an error on generation or execution
	Error error `json:"error"`
}

func NewNonMemModel(modelname string) NonMemModel {

	fs := afero.NewOsFs()
	lm := NonMemModel{}

	if filepath.IsAbs(modelname) {
		lm.Path = modelname
	} else {
		current, err := os.Getwd()
		if err != nil {
			lm.Error = err
		}
		lm.Path = path.Join(current, modelname)
	}

	fi, err := fs.Stat(lm.Path)

	if err != nil {
		return NonMemModel{
			Error: err,
		}
	}

	lm.Model = fi.Name()

	modelPieces := strings.Split(lm.Model, ".")

	lm.FileName = modelPieces[0]

	//Don't assume file will have extension
	if len(modelPieces) > 1 {
		lm.Extension = modelPieces[1]
	}

	//Get the raw path of the original by stripping the actual file from it
	lm.OriginalPath = strings.Replace(lm.Path, "/"+lm.Model, "", 1)

	//Process The template from the viper content for output Dir
	t, err := template.New("output").Parse(viper.GetString("outputDir"))
	buf := new(bytes.Buffer)

	if err != nil {
		return NonMemModel{
			Error: errors.New("There was an issue parsing the template provided: " + err.Error()),
		}
	}

	type outputName struct {
		Name string
	}

	//Make sure to only use the filename for the output dir
	err = t.Execute(buf, outputName{
		Name: lm.FileName,
	})

	if err != nil {
		return NonMemModel{
			Error: err,
		}
	}

	//Use the template content plus the original path
	lm.OutputDir = path.Join(lm.OriginalPath, buf.String())

	if err != nil {
		return NonMemModel{
			Error: errors.New("There was an issue executing the provided template: " + err.Error()),
		}
	}

	lm.Settings = runner.RunSettings{
		Git:                viper.GetBool("git"),
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
func (l NonMemModel) Prepare(channels *turnstile.ChannelMap) {
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
					RunIdentifier: l.Model,
					Error:         err,
					Notes:         "An error occured trying to remove the directory as specified in the overwrite flag",
				}
				return
			}
		} else {
			channels.Failed <- 1
			channels.Errors <- turnstile.ConcurrentError{
				RunIdentifier: l.Model,
				Error:         errors.New("The output directory already exists"),
				Notes:         fmt.Sprintf("The target directory, %s already, exists, but we are configured to not overwrite. Invalid configuration / run state", l.OutputDir),
			}
			return
		}
	}

	//Copy Model into destination and update Data Path
	err := copyFileToDestination(l, true)

	//Now that the directory is created, let's create the gitignore file if specified
	if viper.GetBool("git") {
		WriteGitIgnoreFile(l.OutputDir)
	}

	if err != nil {
		channels.Failed <- 1
		channels.Errors <- turnstile.ConcurrentError{
			RunIdentifier: l.Model,
			Error:         err,
			Notes:         fmt.Sprintf("There appears to have been an issue trying to copy %s to %s", l.Model, l.OutputDir),
		}
		return
	}

	//Create Execution Script
	scriptContents, err := generateScript(scriptTemplate, l)

	if err != nil {
		channels.Failed <- 1
		channels.Errors <- turnstile.ConcurrentError{
			RunIdentifier: l.Model,
			Error:         err,
			Notes:         "An error occurred during the creation of the executable script for this model",
		}
	}

	//rwxr-x---
	afero.WriteFile(fs, path.Join(l.OutputDir, l.FileName+".sh"), scriptContents, 0750)
}

//Work describes the Turnstile execution phase -> IE What heavy lifting should be done
func (l NonMemModel) Work(channels *turnstile.ChannelMap) {
	log.Printf("Beginning work phase for %s", l.FileName)
	fs := afero.NewOsFs()
	//Execute the script we created
	scriptLocation := path.Join(l.OutputDir, l.FileName+".sh")
	os.Chdir(l.OutputDir)

	command := exec.Command(scriptLocation)
	command.Env = os.Environ() //Take in OS Environment

	output, err := command.CombinedOutput()

	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			channels.Failed <- 1
			channels.Errors <- turnstile.ConcurrentError{
				RunIdentifier: l.FileName,
				Error:         err,
				Notes:         "Running the programmatic shell script caused an error",
			}
			code := exitError.ExitCode()
			details := exitError.String()

			log.Printf("Exit code was %d, details were %s", code, details)
			log.Printf("output details were: %s", string(output))
		}

		return
	}

	afero.WriteFile(fs, path.Join(l.OutputDir, l.Model+".out"), output, 0750)

}

//Monitor is unimplemented here. It's the 3rd phase of Turnstile execution
func (l NonMemModel) Monitor(channels *turnstile.ChannelMap) {
	//Do nothing for this implementation
}

//Cleanup is the last phase of execution, in which computation / hard work is done and we're cleaning up leftover files, copying results around et all.
func (l NonMemModel) Cleanup(channels *turnstile.ChannelMap) {
	time.Sleep(10 * time.Millisecond)
	log.Printf("Beginning cleanup phase for model %s\n", l.FileName)
	fs := afero.NewOsFs()

	//Magical instructions
	//TODO: Implement flags for mandatory copy and cleanup exclusions
	pwi := newPostWorkInstruction(l, []string{}, []string{})

	//Copy Up first so that we don't try to move something we remove :)
	var copied []runner.TargetedFile
	//log.Printf("Beginning copy-up operations for model %s\n", l.FileName)
	for _, v := range pwi.FilesToCopy.FilesToCopy {

		source, err := ioutil.ReadFile(path.Join(pwi.FilesToCopy.CopyFrom, v.File))

		if err != nil {
			//Just continue. There are potentially files which will not exist based on the values in the list.
			continue
		}

		//Let's avoid stuttering from extension extrapolation
		file, _ := utils.FileAndExt(v.File)

		if file == l.FileName {
			file = v.File
		} else {
			file = l.FileName + "." + v.File
		}

		err = ioutil.WriteFile(path.Join(pwi.FilesToCopy.CopyTo, file), source, 0755)

		if err != nil {
			log.Printf("An erorr occurred while attempting to copy the files: File is %s", v.File)
			continue
		}

		v.File = l.OutputDir + "/" + v.File

		copied = append(copied, v)
	}

	//Write to File in original path indicating what all was copied
	copiedJSON, _ := json.MarshalIndent(copied, "", "    ")

	afero.WriteFile(fs, path.Join(l.OriginalPath, "copied.json"), copiedJSON, 0750)

	//Clean Up
	//log.Printf("Beginning cleanup operations for model %s\n", l.FileName)
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
				log.Printf("Failure removing file / directory %s. Details : %s", v.File, err.Error())
			}
		}
	}

	//Mark as completed and move on to cleanup
	channels.Completed <- 1
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
	nonmemCmd.AddCommand(localCmd)

	//String Variables
	localCmd.Flags().String("cacheDir", "", "directory path for cache of nonmem executables for NM7.4+")
	viper.BindPFlag("cacheDir", localCmd.Flags().Lookup("cacheDir"))

	localCmd.Flags().String("cacheExe", "", "name of executable stored in cache")
	viper.BindPFlag("cacheExe", localCmd.Flags().Lookup("cacheExe"))

	localCmd.Flags().String("saveExe", "", "what to name the executable when stored in cache")
	viper.BindPFlag("saveExe", localCmd.Flags().Lookup("saveExe"))

	localCmd.Flags().String("outputDir", "{{ .Name }}", "Go template for the output directory to use for storging details of each executed model")
	viper.BindPFlag("outputDir", localCmd.Flags().Lookup("outputDir"))

	//Int Variables
	localCmd.Flags().Int("cleanLvl", 0, "clean level used for file output from a given (set of) runs")
	viper.BindPFlag("cleanLvl", localCmd.Flags().Lookup("cleanLvl"))
	viper.SetDefault("cleanLvl", 0)

	localCmd.Flags().Int("copyLvl", 0, "copy level used for file output from a given (set of) runs")
	viper.BindPFlag("copyLvl", localCmd.Flags().Lookup("copyLvl"))
	viper.SetDefault("copyLvl", 0)

	localCmd.Flags().Int("gitignoreLvl", 0, "gitignore lvl for a given (set of) runs")
	viper.BindPFlag("gitignoreLvl", localCmd.Flags().Lookup("gitignoreLvl"))

	//Bool Variables
	localCmd.Flags().Bool("git", false, "whether git is used")
	viper.BindPFlag("git", localCmd.Flags().Lookup("git"))

	localCmd.Flags().Bool("overwrite", true, "Whether or not to remove existing output directories if they are present")
	viper.BindPFlag("overwrite", localCmd.Flags().Lookup("overwrite"))

}

func local(cmd *cobra.Command, args []string) {

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
				lo.Models = append(lo.Models, NewNonMemModel(model))
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
					lo.Models = append(lo.Models, NewNonMemModel(p))
				}
			} else {
				lo.Models = append(lo.Models, NewNonMemModel(arg))
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
	var errors []NonMemModel
	for _, v := range lo.Models {
		if v.Error != nil {
			counter++
			errors = append(errors, v)
		}
	}

	if counter > 0 {
		log.Printf("It appears that %d models generated an error during the initial setup phase", len(errors))
		for _, v := range errors {
			log.Printf("Model named %s has errored. Details: %s", v.Model, v.Error.Error())
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

//WriteGitIgnoreFile takes a provided path and does best attempt work to write a "Exclude all" gitignore file in the location
func WriteGitIgnoreFile(filepath string) {
	ioutil.WriteFile(path.Join(filepath, ".gitignore"), []byte("*"+lineEnding), 0755)
}
