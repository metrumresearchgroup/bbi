package bbitest

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/metrumresearchgroup/gogridengine"
	"github.com/metrumresearchgroup/wrapt"
	log "github.com/sirupsen/logrus"
)

func TestBbiCompletesSGEExecution(tt *testing.T) {
	t := wrapt.WrapT(tt)

	// Get BB and make sure we have the test data moved over.
	// Clean Slate

	if !FeatureEnabled("SGE") {
		t.Skip("Skipping SGE as it's not enabled")
	}

	scenarios, err := InitializeScenarios([]string{
		"240",
		"acop",
		"ctl_test",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 3)

	// Test shouldn't take longer than 5 min in total
	// TODO use the context downstream in a runModel function
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Minute)
	defer cancel()

	// TODO Break this into a method that takes a function for execution
	for _, v := range scenarios {
		tt.Run(v.identifier, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			t.R.NoError(v.Prepare(ctx))

			for _, m := range v.models {
				tt.Run(m.identifier, func(tt *testing.T) {
					t := wrapt.WrapT(tt)

					nonMemArguments := []string{
						"-d",
						"nonmem",
						"run",
						"sge",
						"--nm_version",
						os.Getenv("NMVERSION"),
					}

					_, err := m.Execute(v, nonMemArguments...)
					t.R.NoError(err)

					WaitForSGEToTerminate(getGridNameIdentifier(m))

					testingDetails := NonMemTestingDetails{
						OutputDir: filepath.Join(v.Workpath, m.identifier),
						Model:     m,
					}

					AssertNonMemCompleted(t, testingDetails)
					AssertNonMemCreatedOutputFiles(t, testingDetails)
					AssertContainsBBIScript(t, testingDetails)
				})
			}
		})
	}
}

func TestBbiCompletesParallelSGEExecution(tt *testing.T) {
	t := wrapt.WrapT(tt)

	// Get BB and make sure we have the test data moved over.
	// Clean Slate

	if !FeatureEnabled("SGE") {
		t.Skip("Skipping SG Parallel execution as it's not enabled")
	}

	scenarios, err := InitializeScenarios([]string{
		"240",
		"acop",
		"ctl_test",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 3)

	// Test shouldn't take longer than 5 min in total
	// TODO use the context downstream in a runModel function
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Minute)
	defer cancel()

	// TODO Break this into a method that takes a function for execution
	for _, v := range scenarios[0:3] {
		tt.Run(v.identifier, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			// log.Infof("Beginning SGE parallel execution test for model set %s",v.identifier)
			t.R.NoError(v.Prepare(ctx))

			for _, m := range v.models {
				tt.Run(m.identifier, func(tt *testing.T) {
					t := wrapt.WrapT(tt)

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

					_, err := m.Execute(v, nonMemArguments...)
					t.R.NoError(err)

					WaitForSGEToTerminate(getGridNameIdentifier(m))

					testingDetails := NonMemTestingDetails{
						OutputDir: filepath.Join(v.Workpath, m.identifier),
						Model:     m,
					}

					AssertNonMemCompleted(t, testingDetails)
					AssertNonMemCreatedOutputFiles(t, testingDetails)
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
