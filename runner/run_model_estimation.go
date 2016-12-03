package runner

import (
	"log"
	"path/filepath"

	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
	"github.com/spf13/viper"
)

// EstimateModel prepares, runs and cleans up a model estimation run
func EstimateModel(fs afero.Fs, modelPath string, verbose bool, debug bool) error {
	modelFile := filepath.Base(modelPath)
	runNum, _ := utils.FileAndExt(modelPath)
	dir, _ := filepath.Abs(filepath.Dir(modelPath))
	dirInfo, _ := afero.ReadDir(fs, dir)
	dirs := utils.ListDirNames(dirInfo)
	newDirSuggestion := FindNextEstDirNum(runNum, dirs, 2)
	if verbose {
		log.Printf("setting up run infrastructure for run %s", runNum)
		log.Printf("base dir: %s", dir)
		log.Printf("new run directory: %s", newDirSuggestion.NextDirName)
	}

	// copy files with any changes
	err := PrepareEstRun(fs, dir, modelFile, newDirSuggestion.NextDirName)
	if err != nil {
		log.Printf("error preparing estimation run: %s", err)
		return err
	}

	// run model
	log.Println("about to start running model")
	RunEstModel(fs, dir, newDirSuggestion.NextDirName, modelFile)

	// clean up after
	log.Println("cleaning up...")
	estDirInfo, _ := afero.ReadDir(fs, filepath.Join(dir, newDirSuggestion.NextDirName))
	fileList := utils.ListFiles(estDirInfo)
	err = CleanEstFolderAndCopyToParent(fs,
		dir,
		runNum,
		newDirSuggestion.NextDirName,
		fileList,
		viper.GetInt("cleanLvl"),
		viper.GetInt("copyLvl"),
		verbose,
		debug,
	)
	if err != nil {
		log.Printf("error cleaning estimation run: %s", err)
		return err
	}
	return nil
}
