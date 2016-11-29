package main

import (
	"fmt"
	"path/filepath"

	"github.com/dpastoor/nonmemutils/runner"
	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
)

func main() {
	AppFs := afero.NewOsFs()

	filePath := "testdata/run001.mod"

	// create a new dir for model estimation
	runNum, fileExt := utils.FileAndExt(filePath)
	dir := filepath.Dir(filePath)
	dirInfo, _ := afero.ReadDir(AppFs, dir)
	dirs := utils.ListDirNames(dirInfo)
	newDirSuggestion := runner.FindNextEstDirNum(runNum, dirs, 2)
	AppFs.MkdirAll(filepath.Join(
		dir,
		newDirSuggestion.NextDirName,
	), 0755)

	// prepare and copy the model to be run001
	fileLines, _ := utils.ReadLinesFS(AppFs, filePath)
	utils.WriteLinesFS(
		AppFs,
		runner.PrepareForExecution(fileLines),
		filepath.Join(
			dir,
			newDirSuggestion.NextDirName,
			fmt.Sprintf("%s%s", runNum, fileExt),
		),
	)

	// dirToClean := newDirSuggestion.NextDirName
	// cleanLvl := 2
	// copyLvl := 2
	// edirInfo, _ := afero.ReadDir(AppFs, filepath.Join(dir, dirToClean))
	// fileList := utils.ListFiles(edirInfo)
	// runner.CleanEstFolderAndCopyToParent(AppFs, dir, runNum, dirToClean, fileList, cleanLvl, copyLvl)
}
