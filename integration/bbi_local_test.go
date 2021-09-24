package bbitest

import (
	"context"
	"io"
	"os"
	"path/filepath"
	"testing"
	"time"

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
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
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
		{name: "240"},
		{name: "acop"},
		{name: "ctl_test"},
		{name: "metrum_std"},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
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

					// Now let's run the script that was generated
					t.R.NoError(os.Chdir(filepath.Join(scenario.Workpath, m.identifier)))
					_, err = executeCommand(ctx, filepath.Join(scenario.Workpath, m.identifier, m.identifier+".sh"))
					t.R.NoError(os.Chdir(whereami))
					t.R.NoError(err)

					testingDetails := NonMemTestingDetails{
						OutputDir: filepath.Join(scenario.Workpath, m.identifier),
						Model:     m,
					}

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
		{name: "240"},
		{name: "acop"},
		{name: "ctl_test"},
		{name: "metrum_std"},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
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
		{name: "240"},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
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

func TestSpecifiedConfigByAbsPathLoaded(tt *testing.T) {
	SkipIfNotEnabled(tt, "LOCAL")

	func() {
		t := wrapt.WrapT(tt)

		fs := afero.NewOsFs()

		if ok, _ := afero.DirExists(fs, filepath.Join(ROOT_EXECUTION_DIR, "meow")); ok {
			t.R.NoError(fs.RemoveAll(filepath.Join(ROOT_EXECUTION_DIR, "meow")))
		}

		t.R.NoError(fs.MkdirAll(filepath.Join(ROOT_EXECUTION_DIR, "meow"), 0755))
		// Copy the bbi file here
		source, _ := fs.Open("bbi.yaml")
		defer t.R.NoError(source.Close())
		dest, _ := fs.Create(filepath.Join(ROOT_EXECUTION_DIR, "meow", "bbi.yaml"))
		defer t.R.NoError(dest.Close())

		_, _ = io.Copy(dest, source)
	}()

	tests := []struct {
		name string
	}{
		{name: "240"},
		// {name: "acop"},
		// {name: "ctl_test"},
		// {name: "metrum_std"},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
			defer cancel()

			scenario := InitializeScenario(t, test.name)

			nonMemArguments := []string{
				"-d",
				"--config",
				filepath.Join(ROOT_EXECUTION_DIR, "meow", "bbi.yaml"),
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

					AssertSpecifiedConfigLoaded(t, nmd, filepath.Join(ROOT_EXECUTION_DIR, "meow", "bbi.yaml"))
				})
			}
		})
	}
}

func TestSpecifiedConfigByRelativePathLoaded(tt *testing.T) {
	SkipIfNotEnabled(tt, "LOCAL")

	t := wrapt.WrapT(tt)

	fs := afero.NewOsFs()

	if ok, _ := afero.DirExists(fs, filepath.Join(ROOT_EXECUTION_DIR, "meow")); ok {
		t.R.NoError(fs.RemoveAll(filepath.Join(ROOT_EXECUTION_DIR, "meow")))
	}

	t.R.NoError(fs.MkdirAll(filepath.Join(ROOT_EXECUTION_DIR, "meow"), 0755))
	// Copy the bbi file here
	source, _ := fs.Open("bbi.yaml")
	defer t.R.NoError(source.Close())
	dest, _ := fs.Create(filepath.Join(ROOT_EXECUTION_DIR, "meow", "bbi.yaml"))
	defer t.R.NoError(dest.Close())

	_, _ = io.Copy(dest, source)

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()
	scenario := InitializeScenario(t, "240")

	// Copy config to /${ROOT_EXECUTION_DIR}/meow/bbi.yaml
	nonMemArguments := []string{
		"-d",
		"--config",
		filepath.Join(ROOT_EXECUTION_DIR, "meow", "bbi.yaml"),
		"nonmem",
		"run",
		"local",
		"--nm_version",
		os.Getenv("NMVERSION"),
	}

	scenario.Prepare(t, ctx)

	for _, v := range scenario.models {
		t.Run(v.identifier, func(t *wrapt.T) {
			out, _ := v.Execute(scenario, nonMemArguments...)
			nmd := NonMemTestingDetails{
				OutputDir: "",
				Model:     v,
				Output:    out,
			}

			AssertSpecifiedConfigLoaded(t, nmd, filepath.Join(ROOT_EXECUTION_DIR, "meow", "bbi.yaml"))
		})
	}
}

func SkipIfNotEnabled(t *testing.T, feature string) {
	if !FeatureEnabled(feature) {
		t.Skip()
	}
}
