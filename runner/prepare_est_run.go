package runner

import (
	"path/filepath"

	"fmt"

	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
)

// PrepareEstRun prepares the directories and files needed to execute a run in the correct subdirectory
// dir is the directory the model file is current in
// runName is the file name of the model
// runDir is the name of the new subdirectory to be created
func PrepareEstRun(fs afero.Fs, dir string, runName string, runDir string) error {
	err := fs.MkdirAll(filepath.Join(
		dir,
		runDir,
	), 0755)
	if err != nil {
		return fmt.Errorf("Error creating new subdir to execute, with err: %s", err)
	}
	fileLines, err := utils.ReadLinesFS(fs, filepath.Join(
		dir,
		runName,
	))
	if err != nil {
		return fmt.Errorf("Error reading in model file, with err: %s", err)
	}
	err = utils.WriteLinesFS(
		fs,
		PrepareForExecution(fileLines),
		filepath.Join(
			dir,
			runDir,
			runName,
		),
	)
	if err != nil {
		return fmt.Errorf("Error copying file to new run dir, with err: %s", err)
	}
	return nil
}
