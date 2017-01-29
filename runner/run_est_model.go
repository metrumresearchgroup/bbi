package runner

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
)

// RunEstModel runs the estimation model in a given model dir
//
// baseDir is the directory from which the original model file was copied
// modelDir is the name of the model directory to run the copied model
// runName is the name of the run model file --> run001.mod
// noBuild is whether to append --nobuild given a nonmem executable is in the run dir
func RunEstModel(fs afero.Fs,
	baseDir string,
	modelDir string,
	runName string,
	noBuild bool,
	nmExecutableOrPath string,
) error {
	modelDirPath := filepath.Join(baseDir, modelDir)
	ok, err := utils.DirExists(modelDirPath, fs)
	if !ok || err != nil {
		//TODO: change these exits to instead just return an error probably
		log.Printf("could not find directory to run model %s, ERR: %s, ok: %v", modelDir, err, ok)
		return err
	}
	runNum, fileExt := utils.FileAndExt(runName)
	nmExecutable := nmExecutableOrPath
	cmdArgs := []string{
		strings.Join([]string{runNum, fileExt}, ""),
		strings.Join([]string{runNum, ".lst"}, ""),
	}

	if noBuild {
		cmdArgs = append(cmdArgs, "--nobuild")
	}

	cmd := exec.Command(nmExecutable, cmdArgs...)
	// set directory for the shell to relevant directory
	cmd.Dir = modelDirPath
	cmdReader, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error creating StdoutPipe for Cmd", err)
	}

	scanner := bufio.NewScanner(cmdReader)
	outputFile, err := os.Create(filepath.Join(modelDirPath, "stdout.out"))
	if err != nil {
		fmt.Fprintln(os.Stderr, "could not make stdout file to pipe output to")
	} else {
		defer outputFile.Close()
	}

	outputFileWriter := bufio.NewWriter(outputFile)

	// handles where to write output to
	go func() {
		for scanner.Scan() {
			fmt.Fprintf(outputFileWriter, "%s out | %s\n", runName, scanner.Text())
		}
	}()

	err = cmd.Start()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error starting Cmd", err)
		return err
	}

	err = cmd.Wait()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error attempting to run model, check the lst file in the run directory for more details", err)
		return err
	}
	outputFileWriter.Flush()
	return nil
}
