package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

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

	// run model
	origDir, _ := os.Getwd()
	fmt.Println("starting at dir: ", origDir)
	err := os.Chdir(filepath.Join(dir, newDirSuggestion.NextDirName))
	if err != nil {
		fmt.Println("could not change directory to: ", newDirSuggestion.NextDirName)
		os.Exit(1)
	}
	cmdName := "nmfe74"
	cmdArgs := []string{
		strings.Join([]string{runNum, fileExt}, ""),
		strings.Join([]string{runNum, ".lst"}, ""),
	}

	cmd := exec.Command(cmdName, cmdArgs...)
	cmdReader, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error creating StdoutPipe for Cmd", err)
		os.Exit(1)
	}

	scanner := bufio.NewScanner(cmdReader)
	go func() {
		for scanner.Scan() {
			fmt.Printf("nmfe74 out | %s\n", scanner.Text())
		}
	}()

	err = cmd.Start()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error starting Cmd", err)
		os.Exit(1)
	}

	err = cmd.Wait()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error waiting for Cmd", err)
		os.Exit(1)
	}

	err = os.Chdir(origDir)
	if err != nil {
		fmt.Println("could not change directory back to: ", origDir)
		os.Exit(1)
	}
	backToDir, _ := os.Getwd()
	fmt.Println("changed dir back to: ", backToDir)

	dirToClean := newDirSuggestion.NextDirName
	cleanLvl := 2
	copyLvl := 2
	edirInfo, _ := afero.ReadDir(AppFs, filepath.Join(dir, dirToClean))
	fileList := utils.ListFiles(edirInfo)
	runner.CleanEstFolderAndCopyToParent(AppFs, dir, runNum, dirToClean, fileList, cleanLvl, copyLvl)
}
