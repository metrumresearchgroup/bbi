package runner

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"path/filepath"
	"strings"

	"github.com/dpastoor/babylon/utils"
	"github.com/spf13/afero"
)

// CopiedFile represents metadata about files copied after run to parent directory
type CopiedFile struct {
	File  string `json:"file,omitempty"`
	Level int    `json:"level,omitempty"`
}

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
	var copiedFiles []CopiedFile
	if !filepath.IsAbs(dirToClean) {
		dirToClean = filepath.Join(parentDir, dirToClean)
	}
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
			copiedFiles = append(copiedFiles, CopiedFile{File: file, Level: lvl})
		}

		if len(copiedFiles) > 0 {
			b, err := json.MarshalIndent(copiedFiles, "", "\t")
			if err != nil {
				log.Println("error marshaling copied files to json")
			}
			copyInfoFile, err := fs.Create(fmt.Sprintf("%s_copied.json", runNum))
			if err != nil {
				log.Printf("error copying to new file: (%s)", err)
			} else {
				copyInfoFile.Write(b)
			}
		}
		// handle cleaning
		lvl, ok = outputFiles[file]
		if debug {
			fmt.Println(fmt.Sprintf("%v: %s --> lvl:  %v ok: %v", i, file, lvl, ok))
		}
		if ok && cleanLvl >= lvl {
			err := fs.Remove(filepath.Join(
				dirToClean,
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
	return nil
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
// runner.CleanEstFolder(AppFs, dirPath, runNum, fileList, cleanLvl, copyLvl, true, true)
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
		fmt.Println(fmt.Sprintf("cleaning folder for run: %v", runName))
	}
	for _, f := range keepFiles {
		// make sure will be kept
		outputFiles[f] = cleanLvl + 1
	}

	// handle temp_dir specially
	lvl, _ := outputFiles["temp_dir"]

	if cleanLvl >= lvl {

		err := fs.RemoveAll(filepath.Join(
			dirPath,
			"temp_dir",
		))
		if err != nil {
			return fmt.Errorf("could not remove temp_dir, %s", err)
		}
	}

	dirInfo, _ := afero.ReadDir(fs, dirPath)
	fileList := utils.ListFiles(dirInfo)
	deletedFiles := 0
	for i, file := range fileList {

		// handle cleaning
		lvl, ok := outputFiles[file]
		if debug {
			fmt.Println(fmt.Sprintf("%v: %s --> lvl:  %v ok: %v", i, file, lvl, ok))
		}
		if ok && cleanLvl >= lvl {
			deletedFiles++
			if preview {
				fmt.Println(fmt.Sprintf("would delete: %s", file))
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
