package runner

import (
	"path/filepath"

	"fmt"

	parser "bbi/parsers/nmparser"
	"bbi/utils"
	"github.com/spf13/afero"
)

// ModFileInfo stores information parsed from a model file such as declared files for use in other parts of the program
type ModFileInfo struct {
	Files []string
}

// PrepareEstRun prepares the directories and files needed to execute a run in the correct subdirectory
// dir is the directory the model file is current in
// runName is the file name of the model
// runDir is the name of the new subdirectory to be created
func PrepareEstRun(fs afero.Fs, dir string, runName string, runDir string) (ModFileInfo, error) {
	if !filepath.IsAbs(runDir) {
		runDir = filepath.Join(
			dir,
			runDir,
		)
	}
	err := fs.MkdirAll(runDir, 0755)
	if err != nil {
		return ModFileInfo{}, fmt.Errorf("Error creating new subdir to execute, with err: %s", err)
	}
	fileLines, err := utils.ReadLinesFS(fs, filepath.Join(
		dir,
		runName,
	))
	if err != nil {
		return ModFileInfo{}, fmt.Errorf("Error reading in model file, with err: %s", err)
	}
	mfi := ModFileInfo{parser.FindOutputFiles(fileLines)}
	err = utils.WriteLinesFS(
		fs,
		PrepareForExecution(fileLines),
		filepath.Join(
			runDir,
			runName,
		),
	)
	if err != nil {
		return mfi, fmt.Errorf("Error copying file to new run dir, with err: %s", err)
	}

	return mfi, nil
}
