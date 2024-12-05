package nonmem

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/metrumresearchgroup/wrapt"
)

func checkGridExecution(
	tt *testing.T, command string, scenarios []string, waitFn func(string),
) {
	tt.Helper()

	for _, s := range scenarios {
		tt.Run(command+s, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			scenario := InitializeScenario(t, s)

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

					waitFn(getGridNameIdentifier(model))

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

func checkParallelGridExecution(
	tt *testing.T, command string, scenarios []string, waitFn func(string),
) {
	tt.Helper()

	for _, s := range scenarios {
		tt.Run(command+s, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			scenario := InitializeScenario(t, s)

			// Test shouldn't take longer than 5 min in total
			// TODO use the context downstream in a runModel function
			ctx, cancel := context.WithTimeout(context.Background(), 30*time.Minute)
			defer cancel()

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

					waitFn(getGridNameIdentifier(m))

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

func getGridNameIdentifier(model Model) string {
	if envValue := os.Getenv("BBI_GRID_NAME_PREFIX"); envValue != "" {
		return envValue + "_Run_" + model.identifier
	} else {
		return "Run_" + model.identifier
	}
}
