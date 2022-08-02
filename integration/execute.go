package bbitest

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"strings"

	log "github.com/sirupsen/logrus"
)

func executeCommand(ctx context.Context, command string, args ...string) (string, error) {
	//Find it in path
	binary, _ := exec.LookPath(command)
	cmd := exec.CommandContext(ctx, binary, args...)
	cmd.Env = os.Environ()
	output, err := cmd.CombinedOutput()

	if err != nil {
		log.Infof("Command was '%s' while arguments were %s", command, strings.Join(args, " "))
		log.Errorf("An error occurred trying to execute model. Error details are : %s", err)

		var exitError *exec.ExitError
		if ok := errors.As(err, &exitError); ok {
			code := exitError.ExitCode()
			details := exitError.String()

			log.Errorf("Exit code was %d, details were %s", code, details)
		}

		log.Errorf("output details were: %s", string(output))

		return string(output), err
	}

	outputString := string(output)

	return outputString, nil
}

// nolint:unparam
func executeCommandNoErrorCheck(ctx context.Context, command string, args ...string) (string, error) {
	binary, _ := exec.LookPath(command)
	cmd := exec.CommandContext(ctx, binary, args...)
	cmd.Env = os.Environ()
	output, err := cmd.CombinedOutput()
	outputString := string(output)

	return outputString, err
}
