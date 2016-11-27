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
	filePath := "fixtures/run001.mod"

	// create a new dir for model estimation
	fileName, fileExt := utils.FileAndExt(filePath)
	dir := filepath.Dir(filePath)
	dirInfo, _ := afero.ReadDir(AppFs, dir)
	dirs := utils.ListDirNames(dirInfo)
	newDirSuggestion := runner.FindNextEstDirNum(fileName, dirs, 2)
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
			fmt.Sprintf("%s%s", fileName, fileExt),
		),
	)
}
