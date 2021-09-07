package runner

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"path/filepath"
	"strings"

	"bbi/utils"

	"github.com/spf13/afero"
)

// TargetedFile represents metadata about files copied or cleaned after run.
type TargetedFile struct {
	File  string `json:"file,omitempty"`
	Level int    `json:"level,omitempty"`
}

// PostWorkInstructions contains all hte details for whether any cleanup or copy actions should occur during the post work phase of the operation.
type PostWorkInstructions struct {
	FilesToCopy  FileCopyInstruction
	FilesToClean FileCleanInstruction
}

// FileCopyInstruction includes everything necessary to copy files back to the source directory.
type FileCopyInstruction struct {
	// The target directory where files will be copied into
	CopyTo string
	// The source directory from which files will be copied
	CopyFrom string
	// Slice of strings identifying files to be copied. Should be processed from clean level + any overrides
	FilesToCopy []TargetedFile
}

// FileCleanInstruction includes everything necessary to remove files for a designated model run.
type FileCleanInstruction struct {
	// Directory in which the files we will remove exist. Should correlate to template output for outputDir
	Location string
	// Slice of strings representing filenames to be purged. Should correlate to clean level contents
	FilesToRemove []TargetedFile
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
// fileList := utils.ListFiles(dirInfo) - Wait if this can be executed, why pass it down stream?
// runner.CleanEstFolderAndCopyToParent(AppFs, dir, runNum, dirToClean, fileList, cleanLvl, copyLvl, true, true).
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
	_ /*verbose*/ bool,
	debug bool,
) error {

	outputFiles := EstOutputFileCleanLevels(runNum)
	keyOutputFiles := EstOutputFilesByRun(runNum)
	var copiedFiles []TargetedFile
	if !filepath.IsAbs(dirToClean) {
		dirToClean = filepath.Join(parentDir, dirToClean)
	}

	// We're literally forcably incrementing the cleanlvl / keep level to make sure they're not operated on?
	for _, f := range keepFiles {
		// make sure will be kept
		outputFiles[f] = cleanLvl + 1
	}
	for _, f := range copyFiles {
		// make sure will be copied
		keyOutputFiles[f] = copyLvl + 1
	}
	// handle temp_dir specially
	lvl := outputFiles["temp_dir"]
	if cleanLvl >= lvl {
		err := fs.RemoveAll(filepath.Join(
			dirToClean,
			"temp_dir",
		))
		if err != nil {
			return fmt.Errorf("removing temp dir: %w", err)
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
				return fmt.Errorf("copying file: %w", err)
			}

			newFileLocation := filepath.Join(
				parentDir,
				file,
			)
			newFile, err := fs.Create(newFileLocation)
			if err != nil {
				return fmt.Errorf("creating new file: %w", err)
			}

			_, err = io.Copy(newFile, fileToCopy)
			if err != nil {
				return fmt.Errorf("copying to new file: %w", err)
			}
			fileToCopy.Close()
			newFile.Close()
			copiedFiles = append(copiedFiles, TargetedFile{File: file, Level: lvl})
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
				_, err = copyInfoFile.Write(b)
				if err != nil {
					log.Printf("couldn't write info file: %s\n", err)
				}
			}
		}
		// handle cleaning
		lvl, ok = outputFiles[file]
		if debug {
			fmt.Printf("%v: %s --> lvl:  %v ok: %v\n", i, file, lvl, ok)
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
