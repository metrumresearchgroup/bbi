package runner

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"bbi/utils"
	"github.com/spf13/afero"
)

// RunEstModel runs the estimation model in a given model dir
//
// modelDir is the name of the model directory to run the copied model
// runName is the name of the run model file --> run001.mod
// noBuild is whether to append --nobuild given a nonmem executable is in the run dir
// nmExecutableOrPath is the name or absolute location of the nmfe executable
func RunEstModel(fs afero.Fs,
	modelDir string,
	runName string,
	noBuild bool,
	nmExecutableOrPath string,
) error {
	ok, err := utils.DirExists(modelDir, fs)
	if !ok || err != nil {
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
	cmd.Dir = modelDir
	cmdReader, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error creating StdoutPipe for Cmd", err)
	}
	errReader, err := cmd.StderrPipe()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error creating StdErrPipe for Cmd", err)
	}

	scanner := bufio.NewScanner(cmdReader)
	errScanner := bufio.NewScanner(errReader)
	outputFile, err := os.Create(filepath.Join(modelDir, "stdout.out"))
	if err != nil {
		fmt.Fprintln(os.Stderr, "could not make stdout file to pipe output to")
	} else {
		defer outputFile.Close()
	}
	errOutputFile, err := os.Create(filepath.Join(modelDir, "stderr.out"))
	if err != nil {
		fmt.Fprintln(os.Stderr, "could not make stderr file to pipe output to")
	} else {
		defer errOutputFile.Close()
	}

	outputFileWriter := bufio.NewWriter(outputFile)
	errOutputFileWriter := bufio.NewWriter(outputFile)

	// handles where to write output to
	go func() {
		for scanner.Scan() {
			//fmt.Fprintf(outputFileWriter, "%s out | %s\n", runName, scanner.Text())
			// the %s out was generally unneeded unless pushing multiple streams
			// out to the cli at once, but that is currently suppressed until
			// a multiwriter is better supported, will just output the stdout itself to
			// the output writter
			fmt.Fprintf(outputFileWriter, "%s\n", scanner.Text())
		}
	}()
	go func() {
		for errScanner.Scan() {
			fmt.Fprintf(errOutputFileWriter, "%s\n", errScanner.Text())
		}
	}()
	// I think defering these here should be reasonable to make sure they flush before
	// returning any errors from the start/wait processes. Originally had these after the
	// potential errors and think I was missing capturing the flushed lines because
	// the main thread was closing too quickly
	defer outputFileWriter.Flush()
	defer errOutputFileWriter.Flush()
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

	return nil
}
