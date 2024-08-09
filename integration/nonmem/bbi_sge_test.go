package nonmem

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/gogridengine"
	"github.com/metrumresearchgroup/wrapt"
	log "github.com/sirupsen/logrus"
)

func TestBbiCompletesSGEExecution(tt *testing.T) {
	if !FeatureEnabled("SGE") {
		tt.Skip("Skipping SGE as it's not enabled")
	}

	tests := []struct {
		name string
	}{
		{name: "acop"},
		{name: "ctl_test"},
		{name: "leading-path-with space"},
	}

	testId := "INT-SGE-001"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			scenario := InitializeScenario(t, test.name)

			// Test shouldn't take longer than 5 min in total
			// TODO use the context downstream in a runModel function
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Minute)
			defer cancel()

			scenario.Prepare(t, ctx)

			for _, model := range scenario.models {
				t.Run(model.identifier, func(t *wrapt.T) {
					nonMemArguments := []string{
						"-d",
						"nonmem",
						"run",
						"sge",
						"--nm_version",
						os.Getenv("NMVERSION"),
					}

					_, err := model.Execute(scenario, nonMemArguments...)
					t.R.NoError(err)

					WaitForSGEToTerminate(getGridNameIdentifier(model))

					testingDetails := NonMemTestingDetails{
						OutputDir: filepath.Join(scenario.Workpath, model.identifier),
						Model:     model,
					}

					AssertNonMemCompleted(t, testingDetails)
					AssertNonMemCreatedOutputFiles(t, testingDetails)
					AssertNonMemCleanedUpFiles(t, testingDetails)
					AssertContainsBBIScript(t, testingDetails)
				})
			}
		})
	}
}

func TestBbiCompletesParallelSGEExecution(tt *testing.T) {
	if !FeatureEnabled("SGE") {
		tt.Skip("Skipping SG Parallel execution as it's not enabled")
	}

	tests := []struct {
		name string
	}{
		{name: "acop"},
		{name: "ctl_test"},
		{name: "leading-path-with space"},
	}

	testId := "INT-SGE-002"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			scenario := InitializeScenario(t, test.name)

			// Test shouldn't take longer than 5 min in total
			// TODO use the context downstream in a runModel function
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Minute)
			defer cancel()

			// log.Infof("Beginning SGE parallel execution test for model set %s",v.identifier)
			scenario.Prepare(t, ctx)

			for _, m := range scenario.models {
				t.Run(m.identifier, func(t *wrapt.T) {
					nonMemArguments := []string{
						"-d",
						"nonmem",
						"run",
						"sge",
						"--nm_version",
						os.Getenv("NMVERSION"),
						"--parallel=true",
						"--mpi_exec_path",
						os.Getenv("MPIEXEC_PATH"),
						"--threads",
						"2",
					}

					_, err := m.Execute(scenario, nonMemArguments...)
					t.R.NoError(err)

					WaitForSGEToTerminate(getGridNameIdentifier(m))

					testingDetails := NonMemTestingDetails{
						OutputDir: filepath.Join(scenario.Workpath, m.identifier),
						Model:     m,
					}

					AssertNonMemCompleted(t, testingDetails)
					AssertNonMemCreatedOutputFiles(t, testingDetails)
					AssertNonMemCleanedUpFiles(t, testingDetails)
					AssertContainsBBIScript(t, testingDetails)
					AssertNonMemOutputContainsParafile(t, testingDetails)
				})
			}
		})
	}
}

/*
func fakeBinary(name string) {
	contents := `#!/bin/bash
	echo $0 $@
	exit 0`

	ioutil.WriteFile(name, []byte(contents), 0755)
}
*/

/*
func purgeBinary(name string) {
	os.Remove(name)
}
*/

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

func getGridNameIdentifier(model Model) string {
	if envValue := os.Getenv("BBI_GRID_NAME_PREFIX"); envValue != "" {
		return envValue + "_Run_" + model.identifier
	} else {
		return "Run_" + model.identifier
	}
}
