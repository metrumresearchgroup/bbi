package nonmem

import (
	"context"
	"io"
	"os"
	"path/filepath"
	"testing"
	"time"

	bi "github.com/metrumresearchgroup/bbi/integration"
	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/wrapt"
	"github.com/spf13/afero"
)

func TestBbiCompletesLocalExecution(tt *testing.T) {
	SkipIfNotEnabled(tt, "LOCAL")

	tests := []struct {
		name string
	}{
		{name: "acop"},
		{name: "metrum_std"},
		{name: "leading-path-with space"},
		{name: "modquote"},
		{name: "period_test"},
		{name: "datacomment"},
	}

	testId := "INT-LOCAL-001"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			scenario := InitializeScenario(t, test.name)

			// Test shouldn't take longer than 5 min in total
			// TODO use the context downstream in a runModel function
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
			defer cancel()

			// TODO Break this into a method that takes a function for execution

			scenario.Prepare(t, ctx)

			for _, m := range scenario.models {
				t.Run(m.identifier, func(t *wrapt.T) {
					nonMemArguments := []string{
						"-d",
						"nonmem",
						"run",
						"local",
						"--nm_version",
						os.Getenv("NMVERSION"),
					}

					_, err := m.Execute(scenario, nonMemArguments...)
					t.R.NoError(err)

					testingDetails := NonMemTestingDetails{
						OutputDir: filepath.Join(scenario.Workpath, m.identifier),
						Model:     m,
						Scenario:  scenario,
					}

					AssertNonMemCompleted(t, testingDetails)
					AssertNonMemCreatedOutputFiles(t, testingDetails)
					AssertNonMemCleanedUpFiles(t, testingDetails)
					AssertContainsBBIScript(t, testingDetails)
					AssertDataSourceIsHashedAndCorrect(t, testingDetails)
					AssertModelIsHashedAndCorrect(t, testingDetails)
				})
			}
		})
	}
}

func TestNMFEOptionsEndInScript(tt *testing.T) {
	SkipIfNotEnabled(tt, "LOCAL")

	tests := []struct {
		name string
	}{
		{name: "acop"},
		{name: "ctl_test"},
		{name: "metrum_std"},
		{name: "leading-path-with space"},
		{name: "modquote"},
	}

	testId := "INT-LOCAL-002"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			scenario := InitializeScenario(t, test.name)

			whereami, err := os.Getwd()
			t.R.NoError(err)

			// Test shouldn't take longer than 5 min in total
			// TODO use the context downstream in a runModel function
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
			defer cancel()

			// TODO Break this into a method that takes a function for execution

			scenario.Prepare(t, ctx)

			for _, m := range scenario.models {
				t.Run(m.identifier, func(t *wrapt.T) {
					nonMemArguments := []string{
						"-d",
						"nonmem",
						"run",
						"local",
						"--nm_version",
						os.Getenv("NMVERSION"),
						"--background=true",
						"--prcompile=true",
					}

					_, err = m.Execute(scenario, nonMemArguments...)
					t.R.NoError(err)

					testingDetails := NonMemTestingDetails{
						OutputDir: filepath.Join(scenario.Workpath, m.identifier),
						Model:     m,
					}

					AssertNonMemCompleted(t, testingDetails)
					AssertNonMemCreatedOutputFiles(t, testingDetails)
					AssertNonMemCleanedUpFiles(t, testingDetails)

					// Now let's run the script that was generated
					t.R.NoError(os.Chdir(filepath.Join(scenario.Workpath, m.identifier)))
					_, err = bi.ExecuteCommand(ctx, filepath.Join(scenario.Workpath, m.identifier, m.identifier+".sh"))
					t.R.NoError(os.Chdir(whereami))
					t.R.NoError(err)

					AssertNonMemCompleted(t, testingDetails)
					AssertNonMemCreatedOutputFiles(t, testingDetails)
					AssertContainsBBIScript(t, testingDetails)
					AssertContainsNMFEOptions(t, filepath.Join(testingDetails.OutputDir, m.identifier+".sh"), "-background")
					AssertContainsNMFEOptions(t, filepath.Join(testingDetails.OutputDir, m.identifier+".sh"), "-prcompile")
				})
			}
		})
	}
}

func TestBbiParallelExecution(tt *testing.T) {
	SkipIfNotEnabled(tt, "LOCAL")
	// Get BB and make sure we have the test data moved over.
	// Clean Slate
	tests := []struct {
		name string
	}{
		{name: "acop"},
		{name: "ctl_test"},
		{name: "metrum_std"},
		{name: "leading-path-with space"},
		{name: "modquote"},
	}

	testId := "INT-LOCAL-003"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)
			scenario := InitializeScenario(t, test.name)

			// Test shouldn't take longer than 5 min in total
			// TODO use the context downstream in a runModel function
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
			defer cancel()

			scenario.Prepare(t, ctx)

			for _, m := range scenario.models {
				tt.Run(m.identifier, func(tt *testing.T) {
					t := wrapt.WrapT(tt)

					nonMemArguments := []string{
						"-d",
						"nonmem",
						"run",
						"local",
						"--nm_version",
						os.Getenv("NMVERSION"),
						"--parallel=true",
						"--mpi_exec_path",
						os.Getenv("MPIEXEC_PATH"),
					}

					_, err := m.Execute(scenario, nonMemArguments...)
					t.R.NoError(err)

					testingDetails := NonMemTestingDetails{
						OutputDir: filepath.Join(scenario.Workpath, m.identifier),
						Model:     m,
						Scenario:  scenario,
					}

					AssertNonMemCompleted(t, testingDetails)
					AssertNonMemCreatedOutputFiles(t, testingDetails)
					AssertNonMemCleanedUpFiles(t, testingDetails)
					AssertContainsBBIScript(t, testingDetails)
					AssertNonMemOutputContainsParafile(t, testingDetails)
					AssertDataSourceIsHashedAndCorrect(t, testingDetails)
					AssertModelIsHashedAndCorrect(t, testingDetails)
				})
			}
		})
	}
}

func TestDefaultConfigLoaded(tt *testing.T) {
	SkipIfNotEnabled(tt, "LOCAL")

	tests := []struct {
		name string
	}{
		{name: "acop"},
	}

	testId := "INT-LOCAL-004"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
			defer cancel()
			scenario := InitializeScenario(t, test.name)

			nonMemArguments := []string{
				"-d",
				"nonmem",
				"run",
				"local",
				"--nm_version",
				os.Getenv("NMVERSION"),
			}

			scenario.Prepare(t, ctx)

			for _, model := range scenario.models {
				t.Run(model.identifier, func(t *wrapt.T) {
					out, _ := model.Execute(scenario, nonMemArguments...)
					nmd := NonMemTestingDetails{
						OutputDir: "",
						Model:     model,
						Output:    out,
					}

					AssertDefaultConfigLoaded(t, nmd)
				})
			}
		})
	}
}

func copyConfig(t *wrapt.T) string {
	t.Helper()

	fs := afero.NewOsFs()

	dir := filepath.Join(ROOT_EXECUTION_DIR, "meow")
	config := filepath.Join(dir, "bbi.yaml")

	if ok, _ := afero.DirExists(fs, dir); ok {
		t.R.NoError(fs.RemoveAll(dir))
	}

	t.R.NoError(fs.MkdirAll(dir, 0755))
	source, err := fs.Open("bbi.yaml")
	t.R.NoError(err)
	defer source.Close()
	dest, err := fs.Create(config)
	t.R.NoError(err)
	defer func() {
		t.R.NoError(dest.Close())
	}()

	_, err = io.Copy(dest, source)
	t.R.NoError(err)

	return config
}

func TestSpecifiedConfigByAbsPathLoaded(tt *testing.T) {
	SkipIfNotEnabled(tt, "LOCAL")

	testId := "INT-LOCAL-005"
	tests := []struct {
		name string
	}{
		{name: "acop"},
		{name: "ctl_test"},
		{name: "metrum_std"},
	}

	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)
			config := copyConfig(t)

			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
			defer cancel()

			scenario := InitializeScenario(t, test.name)

			nonMemArguments := []string{
				"-d",
				"--config",
				config,
				"nonmem",
				"run",
				"local",
				"--nm_version",
				os.Getenv("NMVERSION"),
			}

			scenario.Prepare(t, ctx)

			for _, model := range scenario.models {
				t.Run(model.identifier, func(t *wrapt.T) {
					out, _ := model.Execute(scenario, nonMemArguments...)
					nmd := NonMemTestingDetails{
						OutputDir: "",
						Model:     model,
						Output:    out,
					}

					AssertSpecifiedConfigLoaded(t, nmd, config)
				})
			}
		})
	}
}

func TestSpecifiedConfigByRelativePathLoaded(tt *testing.T) {
	SkipIfNotEnabled(tt, "LOCAL")

	testId := "INT-LOCAL-006"
	tt.Run(utils.AddTestId("", testId), func(tt *testing.T) {
		t := wrapt.WrapT(tt)
		config := copyConfig(t)

		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
		defer cancel()
		scenario := InitializeScenario(t, "acop")

		nonMemArguments := []string{
			"-d",
			"--config",
			config,
			"nonmem",
			"run",
			"local",
			"--nm_version",
			os.Getenv("NMVERSION"),
		}

		scenario.Prepare(t, ctx)

		for _, v := range scenario.models {
			t.Run(utils.AddTestId(v.identifier, testId), func(t *wrapt.T) {
				out, _ := v.Execute(scenario, nonMemArguments...)
				nmd := NonMemTestingDetails{
					OutputDir: "",
					Model:     v,
					Output:    out,
				}

				AssertSpecifiedConfigLoaded(t, nmd, config)
			})
		}
	})
}

func TestLicfileOptionIsRelayed(tt *testing.T) {
	SkipIfNotEnabled(tt, "LOCAL")

	t := wrapt.WrapT(tt)

	dir := t.TempDir()
	nonMemArguments := []string{
		"nonmem",
		"--nm_version",
		os.Getenv("NMVERSION"),
		"run",
		"--licfile", filepath.Join(dir, "dummy-license"),
		"local",
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()
	scenario := InitializeScenario(t, "acop")
	scenario.Prepare(t, ctx)

	for _, m := range scenario.models {
		t.Run(m.identifier, func(t *wrapt.T) {
			_, err := m.Execute(scenario, nonMemArguments...)
			t.R.NoError(err)
			nmd := NonMemTestingDetails{
				OutputDir: filepath.Join(scenario.Workpath, m.identifier),
				Model:     m,
				Scenario:  scenario,
			}

			AssertContainsNMFEOptions(t, filepath.Join(nmd.OutputDir, m.identifier+".sh"),
				"-licfile")
		})
	}
}

func SkipIfNotEnabled(t *testing.T, feature string) {
	if !FeatureEnabled(feature) {
		t.Skip()
	}
}
