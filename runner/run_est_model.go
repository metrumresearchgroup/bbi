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
	"github.com/spf13/viper"
)

// RunEstModel runs the estimation model in a given model dir
// baseDir is the directory from which the original model file was copied
func RunEstModel(fs afero.Fs, baseDir string, modelDir string, runName string) {
	ok, err := utils.DirExists(filepath.Join(baseDir, modelDir), fs)
	if !ok || err != nil {
		//TODO: change these exits to instead just return an error probably
		log.Fatalf("could not change directory to: %s, ERR: %s, ok: %v", modelDir, err, ok)
	}

	runNum, fileExt := utils.FileAndExt(runName)
	nmExecutable := viper.GetString("nmExecutable")
	cmdArgs := []string{
		strings.Join([]string{runNum, fileExt}, ""),
		strings.Join([]string{runNum, ".lst"}, ""),
	}

	cmd := exec.Command(nmExecutable, cmdArgs...)
	// set directory for the shell to relevant directory
	cmd.Dir = filepath.Join(baseDir, modelDir)
	cmdReader, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error creating StdoutPipe for Cmd", err)
		os.Exit(1)
	}

	scanner := bufio.NewScanner(cmdReader)
	go func() {
		for scanner.Scan() {
			fmt.Printf("%s out | %s\n", runName, scanner.Text())
		}
	}()

	err = cmd.Start()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error starting Cmd", err)
		os.Exit(1)
	}

	err = cmd.Wait()
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error attempting to run model, check the lst file in the run directory for more details", err)
		os.Exit(1)
	}
}
