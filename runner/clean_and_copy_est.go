package runner

import (
	"fmt"
	"log"
	"path/filepath"
	"strings"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/spf13/afero"
)

// TargetedFile represents metadata about files copied or cleaned after run.
type TargetedFile struct {
	File  string `json:"file,omitempty"`
	Level int    `json:"level,omitempty"`
}

// PostWorkInstructions contains all hte details for whether any cleanup or copy actions should occur during the post work phase of the operation.
type PostWorkInstructions struct {
	FilesToClean FileCleanInstruction
}

// FileCleanInstruction includes everything necessary to remove files for a designated model run.
type FileCleanInstruction struct {
	// Directory in which the files we will remove exist. Should correlate to template output for outputDir
	Location string
	// Slice of strings representing filenames to be purged. Should correlate to clean level contents
	FilesToRemove []TargetedFile
}

// CleanEstFolder cleans the estimation folder based on the set clean level
// Ex:
// AppFs := afero.NewOsFs()
// runNum := "run001"
// dirPath := "./modeling/run001_est_03"
// cleanLvl := 2
// copyLvl := 2
// dirInfo, _ := afero.ReadDir(AppFs, filepath.Join(dir, dirToClean))
// fileList := utils.ListFiles(dirInfo)
// runner.CleanEstFolder(AppFs, dirPath, runNum, fileList, cleanLvl, copyLvl, true, true).
func CleanEstFolder(
	fs afero.Fs,
	dirPath string,
	keepFiles []string,
	cleanLvl int,
	verbose bool,
	debug bool,
	preview bool,
) error {

	runPath := filepath.Base(dirPath)
	runName := strings.Split(runPath, "_est_")[0]
	outputFiles := EstOutputFileCleanLevels(runName)
	dirPath, err := filepath.Abs(dirPath)
	if err != nil {
		log.Fatalf("could not get absolute path %s", dirPath)
	}
	if debug {
		fmt.Printf("cleaning folder for run: %v\n", runName)
	}
	for _, f := range keepFiles {
		// make sure will be kept
		outputFiles[f] = cleanLvl + 1
	}

	// handle temp_dir specially
	lvl := outputFiles["temp_dir"]

	if cleanLvl >= lvl {
		err := fs.RemoveAll(filepath.Join(
			dirPath,
			"temp_dir",
		))
		if err != nil {
			return fmt.Errorf("removing temp_dir, %w", err)
		}
	}

	dirInfo, _ := afero.ReadDir(fs, dirPath)
	fileList := utils.ListFiles(dirInfo)
	deletedFiles := 0
	for i, file := range fileList {
		// handle cleaning
		lvl, ok := outputFiles[file]
		if debug {
			fmt.Printf("%v: %s --> lvl:  %v ok: %v\n", i, file, lvl, ok)
		}
		if ok && cleanLvl >= lvl {
			deletedFiles++
			if preview {
				fmt.Printf("would delete: %s\n", file)
			} else {
				err := fs.Remove(filepath.Join(
					dirPath,
					file,
				))
				if err != nil {
					return err
				}
				if debug {
					log.Println("deleted file: ", file)
				}
			}
		}
	}
	if verbose {
		log.Printf("number of files cleaned: %v", deletedFiles)
	}

	return nil
}
