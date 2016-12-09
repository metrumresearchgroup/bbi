package runner

import (
	"log"
	"path/filepath"

	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
	"github.com/spf13/viper"
)

// RunSettings is a struct that contains settings about run information
// passed in from config variables or flags
type RunSettings struct {
	Git     bool
	SaveExe string
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
	verbose bool,
	debug bool,
	cacheDir string,
	exeNameInCache string,
) error {
	modelFile := filepath.Base(modelPath)
	runNum, _ := utils.FileAndExt(modelPath)
	dir, _ := filepath.Abs(filepath.Dir(modelPath))
	dirInfo, _ := afero.ReadDir(fs, dir)
	dirs := utils.ListDirNames(dirInfo)
	newDirSuggestion := FindNextEstDirNum(runNum, dirs, 2)
	modelDir := newDirSuggestion.NextDirName
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
		utils.SetupCacheForRun(fs, dir, modelDir, cacheDir, exeNameInCache)
		noBuild = true
	}

	// run model
	if verbose {
		log.Printf("running model %s ...\n", runNum)
	}
	err = RunEstModel(fs, dir, newDirSuggestion.NextDirName, modelFile, noBuild)

	if err != nil {
		log.Printf("error during estimation run: %s", err)
		return err
	}

	if runSettings.SaveExe != "" {
		utils.CopyExeToCache(fs, dir, modelDir, cacheDir, runSettings.SaveExe)
	}

	// clean up after
	if verbose {
		log.Println("cleaning up...")
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
		viper.GetInt("cleanLvl"),
		viper.GetInt("copyLvl"),
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
