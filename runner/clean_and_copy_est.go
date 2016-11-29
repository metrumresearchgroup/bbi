package runner

import (
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/spf13/afero"
)

// CleanEstFolderAndCopyToParent cleans the estimation folder and then copies relevant files back to parent dir
func CleanEstFolderAndCopyToParent(fs afero.Fs, parentDir string, runNum string, dirToClean string, fileList []string, cleanLvl int, copyLvl int) {
	outputFiles := EstOutputFileCleanLevels()
	keyOutputFiles := EstOutputFilesByRun(runNum)
	for i, file := range fileList {

		// handle cleaning
		lvl, ok := outputFiles[file]
		fmt.Println(fmt.Sprintf("%v: %s --> lvl:  %v ok: %v", i, file, lvl, ok))
		if ok && cleanLvl >= lvl {
			err := fs.Remove(filepath.Join(
				parentDir,
				dirToClean,
				file,
			))
			if err != nil {
				fmt.Println("ERROR: ", err)
			}
			fmt.Println("deleted file: ", file)
			continue
		}

		// Copy files to directory above
		lvl, ok = keyOutputFiles[file]
		if ok && lvl >= copyLvl {
			fileToCopyLocation := filepath.Join(
				parentDir,
				dirToClean,
				file,
			)
			fileToCopy, err := fs.Open(fileToCopyLocation)
			if err != nil {
				fmt.Println("TERRIBLE ERROR opening FILE TO COPY")
				os.Exit(1)
			}
			defer fileToCopy.Close()

			newFileLocation := filepath.Join(
				parentDir,
				file,
			)
			newFile, err := fs.Create(newFileLocation)
			if err != nil {
				fmt.Println("TERRIBLE ERROR CREATING FILE TO COPY")
				os.Exit(1)
				continue
			}
			defer newFile.Close()

			_, err = io.Copy(newFile, fileToCopy)
			if err != nil {
				fmt.Println("TERRIBLE ERROR TRYING TO COPY: ", err)
				os.Exit(1)
			}
		}
	}
}
