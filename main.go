package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/dpastoor/nonmemutils/runner"
	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
)

func main() {
	AppFs := afero.NewOsFs()
	runNum := "run001"
	dir := "fixtures"
	dirToClean := "fixtures/run001_est_03/"
	cleanLvl := 2
	dirInfo, _ := afero.ReadDir(AppFs, dirToClean)
	fileList := utils.ListFiles(dirInfo)
	outputFiles := runner.EstOutputFileCleanLevels()
	keyOutputFiles := runner.EstOutputFilesByRun(runNum)
	for i, file := range fileList {
		lvl, ok := outputFiles[file]
		fmt.Println(fmt.Sprintf("%v: %s --> lvl:  %v ok: %v", i, file, lvl, ok))
		if ok && cleanLvl >= lvl {
			err := AppFs.Remove(filepath.Join(
				dirToClean,
				file,
			))
			if err != nil {
				fmt.Println("ERROR: ", err)
			}
			fmt.Println("deleted file: ", file)
			continue
		}
		lvl, ok = keyOutputFiles[file]
		if ok {
			// Copy files to directory above
			fileToCopyLocation := filepath.Join(
				dirToClean,
				file,
			)
			fileToCopy, err := AppFs.Open(fileToCopyLocation)
			if err != nil {
				fmt.Println("TERRIBLE ERROR opening FILE TO COPY")
				os.Exit(1)
			}
			defer fileToCopy.Close()

			newFileLocation := filepath.Join(
				dir,
				file,
			)
			newFile, err := AppFs.Create(newFileLocation)
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
	// create a new dir for model estimation
}
