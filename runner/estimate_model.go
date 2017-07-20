package runner

import (
	"log"
	"path/filepath"

	"fmt"

	"github.com/dpastoor/babylon/utils"
	"github.com/spf13/afero"
)

// RunSettings is a struct that contains settings about run information
// passed in from config variables or flags
type RunSettings struct {
	Git                bool
	SaveExe            string
	Verbose            bool
	Debug              bool
	CleanLvl           int
	CopyLvl            int
	CacheDir           string
	ExeNameInCache     string
	NmExecutableOrPath string
	OneEst             bool
	ProposedRunDir     string
}

// ReturnStatus gives information about the result of a model run
type ReturnStatus struct {
	RunDir string
	DidRun bool
	Error  error
}

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
	runNum, _ := utils.FileAndExt(modelPath)
	dir, _ := filepath.Abs(filepath.Dir(modelPath))
	dirInfo, _ := afero.ReadDir(fs, dir)
	dirs := utils.ListDirNames(dirInfo)
	newDirSuggestion := NextDirSuggestion{}
	var modelDir string
	if runSettings.ProposedRunDir == "" {
		newDirSuggestion = FindNextEstDirNum(runNum, dirs, 2)
		// to normalize with a potentially passed proposedRunDir, will
		// use absolute paths to run models
		modelDir = filepath.Join(modelPath, newDirSuggestion.NextDirName)
	} else {
		// if user proposes a new directory, will consider it a 'first' run, as they are customizing the output
		// directory. As we have less control over this directory, will need to more aggressively check it
		// the default behavior can be considered that it will _not_ overwrite an existing directory,
		// perhaps could add a force/overwrite setting, but for now, will just make the user cleanup if they
		// want a specific directory
		proposedRunDir := runSettings.ProposedRunDir
		if !filepath.IsAbs(proposedRunDir) {
			proposedRunDir := filepath.Join(modelPath, proposedRunDir)
		}
		modelDir = proposedRunDir
		exists, err := utils.DirExists(modelDir, fs)
		if exists {
			return ReturnStatus{
				RunDir: modelDir,
				DidRun: false,
				Error:  fmt.Errorf("run directory %s already exists! will not overwrite", proposedRunDir),
			}
		}
	}

	if !newDirSuggestion.FirstRun && runSettings.OneEst {
		if runSettings.Verbose {
			log.Printf("run directory for model %s exists, not re-running", runNum)
		}
		return ReturnStatus{
			RunDir: modelDir,
			DidRun: false,
			Error:  fmt.Errorf("run directory %s already exists! will not overwrite", newDirSuggestion.NextDirName),
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
		log.Printf("setting up run infrastructure for run %s", runNum)
		log.Printf("base dir: %s", dir)
		log.Printf("new run directory to be prepared: %s", modelDir)
	}

	// copy files with any changes
	extraFiles, err := PrepareEstRun(fs, dir, modelFile, modelDir)
	if err != nil {
		log.Printf("error preparing estimation run: %s", err)
		return ReturnStatus{
			RunDir: modelDir,
			DidRun: false,
			Error:  err,
		}
	}

	if runSettings.Git {
		// for now just add a gitignore to add everything in run output by default
		utils.WriteLinesFS(fs, []string{"*"}, filepath.Join(dir, modelDir, ".gitignore"))
	}
	// deal with cache maybe
	noBuild := false
	if exeNameInCache != "" {
		err := utils.SetupCacheForRun(fs, dir, modelDir, cacheDir, exeNameInCache, debug)
		if err != nil {
			log.Printf("error setting up cache: %v \n unable to precompile model", err)
		} else {
			noBuild = true
		}
	}

	// run model
	if verbose {
		log.Printf("running model %s ...\n", runNum)
	}
	err = RunEstModel(fs, dir, modelDir, modelFile, noBuild, nmExecutableOrPath)

	if err != nil {
		log.Printf("error during estimation run: %s", err)
		return ReturnStatus{
			RunDir: modelDir,
			DidRun: true,
			Error:  err,
		}
	}

	if verbose {
		log.Printf("completed execution of %s, cleaning up...", runNum)
	}

	if runSettings.SaveExe != "" {
		err := utils.CopyExeToCache(fs, dir, modelDir, cacheDir, runSettings.SaveExe)
		if err != nil {
			log.Printf("error during estimation run: %s", err)
		}
	}

	estDirInfo, _ := afero.ReadDir(fs, modelDir)
	fileList := utils.ListFiles(estDirInfo)
	err = CleanEstFolderAndCopyToParent(fs,
		dir,
		runNum,
		modelDir,
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
			RunDir: modelDir,
			DidRun: true,
			Error:  err,
		}
	}
	if verbose {
		log.Println("done cleaning up!")
	}
	return ReturnStatus{
		RunDir: modelDir,
		DidRun: true,
		Error:  nil,
	}
}
