package nonmem

import (
	"bytes"
	"os/exec"
	"testing"
	"time"

	log "github.com/sirupsen/logrus"
)

func TestBbiCompletesSlurmExecution(tt *testing.T) {
	if !FeatureEnabled("SLURM") {
		tt.Skip("Slurm is not enabled")
	}

	tests := []string{
		"acop",
		"ctl_test",
		"leading-path-with space",
		"modquote",
	}

	checkGridExecution(tt, "slurm", tests, WaitForSlurmToTerminate)
}

func TestBbiCompletesParallelSlurmExecution(tt *testing.T) {
	if !FeatureEnabled("SLURM") {
		tt.Skip("Slurm is not enabled")
	}

	tests := []string{
		"acop",
		"ctl_test",
		"leading-path-with space",
		"modquote",
	}

	checkParallelGridExecution(tt, "slurm", tests, WaitForSlurmToTerminate)
}

func WaitForSlurmToTerminate(name string) error {
	log.Infof("waiting for Slurm job %s", name)
	secs := 30 * time.Second
	for {
		time.Sleep(secs)
		contains, err := squeueContains(name)
		if err != nil {
			return err
		}

		if !contains {
			break
		}

		log.Infof("%s is still in squeue output; checking again in %s", name, secs)
	}

	return nil
}

func squeueContains(name string) (bool, error) {
	cmd := exec.Command("squeue", "--noheader", "--format=%i", "--name="+name)
	out, err := cmd.Output()
	if err != nil {
		return false, err
	}

	return len(bytes.TrimSpace(out)) > 0, nil
}
