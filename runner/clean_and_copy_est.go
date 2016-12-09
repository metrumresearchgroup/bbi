package runner

import (
	"fmt"
	"io"
	"log"
	"path/filepath"

	"github.com/spf13/afero"
)

// CleanEstFolderAndCopyToParent cleans the estimation folder and then copies relevant files back to parent dir
// Ex:
// AppFs := afero.NewOsFs()
// runNum := "run001"
// dir := "fixtures"
// dirToClean := "run001_est_03"
// cleanLvl := 2
// copyLvl := 2
// dirInfo, _ := afero.ReadDir(AppFs, filepath.Join(dir, dirToClean))
// fileList := utils.ListFiles(dirInfo)
// runner.CleanEstFolderAndCopyToParent(AppFs, dir, runNum, dirToClean, fileList, cleanLvl, copyLvl, true, true)
func CleanEstFolderAndCopyToParent(
	fs afero.Fs,
	parentDir string,
	runNum string,
	dirToClean string,
	fileList []string,
	keepFiles []string,
	copyFiles []string,
	cleanLvl int,
	copyLvl int,
	verbose bool,
	debug bool,
) error {
	outputFiles := EstOutputFileCleanLevels(runNum)
	keyOutputFiles := EstOutputFilesByRun(runNum)
	for _, f := range keepFiles {
		// make sure will be kept
		outputFiles[f] = cleanLvl + 1
	}
	for _, f := range copyFiles {
		// make sure will be copied
		keyOutputFiles[f] = copyLvl + 1
	}
	// handle temp_dir specially
	lvl, _ := outputFiles["temp_dir"]
	if cleanLvl >= lvl {
		err := fs.RemoveAll(filepath.Join(
			parentDir,
			dirToClean,
			"temp_dir",
		))
		if err != nil {
			return fmt.Errorf("could not remove temp_dir, %s", err)
		}
	}

	for i, file := range fileList {

		// Copy files to directory above
		lvl, ok := keyOutputFiles[file]
		if ok && lvl >= copyLvl {
			fileToCopyLocation := filepath.Join(
				parentDir,
				dirToClean,
				file,
			)
			fileToCopy, err := fs.Open(fileToCopyLocation)
			if err != nil {
				return fmt.Errorf("error copying file: (%s)", err)
			}

			newFileLocation := filepath.Join(
				parentDir,
				file,
			)
			newFile, err := fs.Create(newFileLocation)
			if err != nil {
				return fmt.Errorf("error creating new file: (%s)", err)
			}

			_, err = io.Copy(newFile, fileToCopy)
			if err != nil {
				return fmt.Errorf("error copying to new file: (%s)", err)
			}
			fileToCopy.Close()
			newFile.Close()
		}

		// handle cleaning
		lvl, ok = outputFiles[file]
		if debug {
			fmt.Println(fmt.Sprintf("%v: %s --> lvl:  %v ok: %v", i, file, lvl, ok))
		}
		if ok && cleanLvl >= lvl {
			err := fs.Remove(filepath.Join(
				parentDir,
				dirToClean,
				file,
			))
			if err != nil {
				return err
			}
			if verbose {
				log.Println("deleted file: ", file)
			}
		}

	}
	return nil
}
