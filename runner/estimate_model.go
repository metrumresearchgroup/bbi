package runner

import (
	"log"
	"path/filepath"

	"github.com/dpastoor/nonmemutils/utils"
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
) error {

	modelFile := filepath.Base(modelPath)
	runNum, _ := utils.FileAndExt(modelPath)
	dir, _ := filepath.Abs(filepath.Dir(modelPath))
	dirInfo, _ := afero.ReadDir(fs, dir)
	dirs := utils.ListDirNames(dirInfo)
	newDirSuggestion := FindNextEstDirNum(runNum, dirs, 2)
	modelDir := newDirSuggestion.NextDirName

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
		return err
	}

	if runSettings.Git {
		// for now just add a gitignore to add everything in run output by default
		utils.WriteLinesFS(fs, []string{"*"}, filepath.Join(dir, modelDir, ".gitignore"))
	}
	// deal with cache maybe
	noBuild := false
	if exeNameInCache != "" {
		err := utils.SetupCacheForRun(fs, dir, modelDir, cacheDir, exeNameInCache)
		if err != nil {
			noBuild = true
		} else {
			log.Printf("error setting up cache: %s \n unable to precompile model", err)
		}
	}

	// run model
	log.Printf("running model %s ...\n", runNum)
	err = RunEstModel(fs, dir, newDirSuggestion.NextDirName, modelFile, noBuild, nmExecutableOrPath)

	if err != nil {
		log.Printf("error during estimation run: %s", err)
		return err
	}

	log.Printf("completed execution of %s, cleaning up...", runNum)

	if runSettings.SaveExe != "" {
		utils.CopyExeToCache(fs, dir, modelDir, cacheDir, runSettings.SaveExe)
	}

	estDirInfo, _ := afero.ReadDir(fs, filepath.Join(dir, newDirSuggestion.NextDirName))
	fileList := utils.ListFiles(estDirInfo)
	err = CleanEstFolderAndCopyToParent(fs,
		dir,
		runNum,
		newDirSuggestion.NextDirName,
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
		return err
	}
	if verbose {
		log.Println("done cleaning up!")
	}
	return nil
}
