package nonmem

import (
	"fmt"
	"testing"
	"time"

	"github.com/metrumresearchgroup/gogridengine"
	log "github.com/sirupsen/logrus"
)

func TestBbiCompletesSGEExecution(tt *testing.T) {
	if !FeatureEnabled("SGE") {
		tt.Skip("Skipping SGE as it's not enabled")
	}

	tests := []string{
		"acop",
		"ctl_test",
		"leading-path-with space",
	}

	checkGridExecution(tt, "sge", tests, WaitForSGEToTerminate)
}

func TestBbiCompletesParallelSGEExecution(tt *testing.T) {
	if !FeatureEnabled("SGE") {
		tt.Skip("Skipping SG Parallel execution as it's not enabled")
	}

	tests := []string{
		"acop",
		"ctl_test",
		"leading-path-with space",
	}

	checkParallelGridExecution(tt, "sge", tests, WaitForSGEToTerminate)
}

func WaitForSGEToTerminate(gridNameIdentifier string) {
	log.Info(fmt.Sprintf("Provided value for location job by name was : %s", gridNameIdentifier))
	for CountOfPendingJobs(gridNameIdentifier) > 0 {
		log.Infof("Located %d pending jobs. Waiting for 30 seconds to check again", CountOfPendingJobs(gridNameIdentifier))
		time.Sleep(30 * time.Second)
	}

	log.Info("Looks like all queued and running jobs have terminated")
}

func CountOfPendingJobs(gridNameIdentifier string) int {
	jobs, _ := gogridengine.GetJobsWithFilter(func(j gogridengine.Job) bool {
		return j.JobName == gridNameIdentifier && (j.State == "qw" || j.State == "r")
	})

	return len(jobs)
}
