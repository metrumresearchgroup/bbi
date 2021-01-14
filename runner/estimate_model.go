package runner

import (
	"bytes"
	"log"
	"path/filepath"
	"text/template"

	"errors"

	"bbi/utils"
	"github.com/spf13/afero"
)

// EstimateModel prepares, runs and cleans up a model estimation run
// modelPath is the relative path of the model from where Estimate model is called from
// cacheDir is the location of the cache dir, relative to the baseDir,
//		for nonmem executable for version 7.4 for use in precompilation
// exeNameInCache is the name of the nonmem executable in the cache dir
func EstimateModel(
	fs afero.Fs,
	modelPath string,
	runSettings RunSettings,
) ReturnStatus {

	modelFile := filepath.Base(modelPath)
	//Renaming to fileName to be a bit more explicit
	fileName, _ := utils.FileAndExt(modelPath)
	dir, _ := filepath.Abs(filepath.Dir(modelPath))

	outputDirectoryName, err := processDirectoryTemplate(fileName, runSettings)

	//Need to get the absolute path for the directory
	outputDirectoryPath := filepath.Join(dir, outputDirectoryName)

	isDirectory, _ := afero.IsDir(fs, outputDirectoryPath)
	isEmpty, _ := afero.IsEmpty(fs, outputDirectoryPath)

	//The desired directory is present, but not empty.
	if isDirectory && !isEmpty {

		if runSettings.Overwrite {
			//Let's remove the entire friggin dir and its contents
			log.Print("Settings are configured to over write. Removing existing directory")
			fs.RemoveAll(outputDirectoryPath)
		} else {
			//Generate an error and bubble it up to the user
			return ReturnStatus{
				RunDir: outputDirectoryPath,
				DidRun: false,
				Error:  errors.New("Run settings indicate that we should not overwrite output directories, but the targeted directory," + outputDirectoryPath + " already exists"),
			}
		}
	}

	// unpack settings
	verbose := runSettings.Verbose
	debug := runSettings.Debug
	cleanLvl := runSettings.CleanLvl
	copyLvl := runSettings.CopyLvl
	cacheDir := runSettings.CacheDir
	exeNameInCache := runSettings.ExeNameInCache
	nmExecutableOrPath := runSettings.NmExecutableOrPath

	if verbose {
		log.Printf("setting up run infrastructure for run %s", fileName)
		log.Printf("base dir: %s", dir)
		log.Printf("new run directory to be prepared: %s", outputDirectoryPath)
	}

	// copy files with any changes
	extraFiles, err := PrepareEstRun(fs, dir, modelFile, outputDirectoryPath)
	if err != nil {
		log.Printf("error preparing estimation run: %s", err)
		return ReturnStatus{
			RunDir: outputDirectoryPath,
			DidRun: false,
			Error:  err,
		}
	}

	if runSettings.Git {
		// for now just add a gitignore to add everything in run output by default
		utils.WriteLinesFS(fs, []string{"*"}, filepath.Join(dir, outputDirectoryPath, ".gitignore"))
	}
	// deal with cache maybe
	noBuild := false
	if exeNameInCache != "" {
		err := utils.SetupCacheForRun(fs, dir, outputDirectoryPath, cacheDir, exeNameInCache, debug)
		if err != nil {
			log.Printf("error setting up cache: %v \n unable to precompile model", err)
		} else {
			noBuild = true
		}
	}

	// run model
	if verbose {
		log.Printf("running model %s ...\n", fileName)
	}
	err = RunEstModel(fs, outputDirectoryPath, modelFile, noBuild, nmExecutableOrPath)

	if err != nil {
		log.Printf("error during estimation run: %s", err)
		return ReturnStatus{
			RunDir: outputDirectoryPath,
			DidRun: true,
			Error:  err,
		}
	}

	if verbose {
		log.Printf("completed execution of %s, cleaning up...", fileName)
	}

	if runSettings.SaveExe != "" {
		err := utils.CopyExeToCache(fs, outputDirectoryPath, cacheDir, runSettings.SaveExe)
		if err != nil {
			log.Printf("error during estimation run: %s", err)
		}
	}

	estDirInfo, _ := afero.ReadDir(fs, outputDirectoryPath)
	fileList := utils.ListFiles(estDirInfo)
	err = CleanEstFolderAndCopyToParent(fs,
		dir,
		fileName,
		outputDirectoryPath,
		fileList,
		extraFiles.Files,
		extraFiles.Files,
		cleanLvl,
		copyLvl,
		verbose,
		debug,
	)
	if err != nil {
		log.Printf("error cleaning estimation run: %s", err)
		return ReturnStatus{
			RunDir: outputDirectoryPath,
			DidRun: true,
			Error:  err,
		}
	}
	if verbose {
		log.Println("done cleaning up!")
	}
	return ReturnStatus{
		RunDir: outputDirectoryPath,
		DidRun: true,
		Error:  nil,
	}
}

//Processes the go template for the output directory and returns the processed string and any relative errors
// name: The name of the model from which the template will be derived.
// r: The RunSettings struct containing the configurations from the run.
func processDirectoryTemplate(name string, r RunSettings) (string, error) {
	var generated []byte
	buf := bytes.NewBuffer(generated)

	type templateContent struct {
		Name string
	}

	tc := templateContent{
		Name: name,
	}

	t, err := template.New("outputfile").Parse(r.OutputDir)

	if err != nil {
		return "", errors.New("There was an issue parsing the output directory template as provided by the run settings: " + err.Error())
	}

	err = t.Execute(buf, tc)

	if err != nil {
		return "", errors.New("There was an issue processing the output directory template as provided by the run settings: " + err.Error())
	}

	return buf.String(), nil
}
