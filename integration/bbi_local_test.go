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
	t := wrapt.WrapT(tt)

	SkipIfNotEnabled(t, "LOCAL")

	// Get BB and make sure we have the test data moved over.
	// Clean Slate
	scenarios, err := InitializeScenarios([]string{
		"240",
		"acop",
		"ctl_test",
		"metrum_std",
		"period_test",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 5)

	// Test shouldn't take longer than 5 min in total
	// TODO use the context downstream in a runModel function
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()

	// TODO Break this into a method that takes a function for execution
	for _, v := range scenarios {
		err = v.Prepare(ctx)
		t.R.NoError(err)

		for _, m := range v.models {
			nonMemArguments := []string{
				"-d",
				"nonmem",
				"run",
				"local",
				"--nm_version",
				os.Getenv("NMVERSION"),
			}

			_, err := m.Execute(v, nonMemArguments...)

			t.R.NoError(err)

			testingDetails := NonMemTestingDetails{
				OutputDir: filepath.Join(v.Workpath, m.identifier),
				Model:     m,
				Scenario:  v,
			}

			AssertNonMemCompleted(t, testingDetails)
			AssertNonMemCreatedOutputFiles(t, testingDetails)
			AssertContainsBBIScript(t, testingDetails)
			AssertDataSourceIsHashedAndCorrect(t, testingDetails)
			AssertModelIsHashedAndCorrect(t, testingDetails)
		}
	}
}

func TestNMFEOptionsEndInScript(tt *testing.T) {
	t := wrapt.WrapT(tt)

	SkipIfNotEnabled(t, "LOCAL")
	scenarios, err := InitializeScenarios([]string{
		"240",
		"acop",
		"ctl_test",
		"metrum_std",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 4)

	whereami, _ := os.Getwd()

	// Test shouldn't take longer than 5 min in total
	// TODO use the context downstream in a runModel function
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()

	// TODO Break this into a method that takes a function for execution
	for _, v := range scenarios {
		t.R.NoError(v.Prepare(ctx))

		for _, m := range v.models {
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

			_, err := m.Execute(v, nonMemArguments...)
			t.R.NoError(err)

			// Now let's run the script that was generated
			t.R.NoError(os.Chdir(filepath.Join(v.Workpath, m.identifier)))
			_, err = executeCommand(ctx, filepath.Join(v.Workpath, m.identifier, m.identifier+".sh"))
			t.R.NoError(os.Chdir(whereami))
			t.R.NoError(err)

			testingDetails := NonMemTestingDetails{
				OutputDir: filepath.Join(v.Workpath, m.identifier),
				Model:     m,
			}

			AssertNonMemCompleted(t, testingDetails)
			AssertNonMemCreatedOutputFiles(t, testingDetails)
			AssertContainsBBIScript(t, testingDetails)
			AssertContainsNMFEOptions(t, filepath.Join(testingDetails.OutputDir, m.identifier+".sh"), "-background")
			AssertContainsNMFEOptions(t, filepath.Join(testingDetails.OutputDir, m.identifier+".sh"), "-prcompile")
		}
	}
}

func TestBbiParallelExecution(tt *testing.T) {
	t := wrapt.WrapT(tt)

	SkipIfNotEnabled(t, "LOCAL")
	// Get BB and make sure we have the test data moved over.
	// Clean Slate
	scenarios, err := InitializeScenarios([]string{
		"240",
		"acop",
		"ctl_test",
		"metrum_std",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 4)

	// Test shouldn't take longer than 5 min in total
	// TODO use the context downstream in a runModel function
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()

	// TODO Break this into a method that takes a function for execution
	for _, v := range scenarios {
		// log.Infof("Beginning localized parallel execution test for model set %s",v.identifier)
		t.R.NoError(v.Prepare(ctx))

		for _, m := range v.models {
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

			_, err := m.Execute(v, nonMemArguments...)
			t.R.NoError(err)

			testingDetails := NonMemTestingDetails{
				OutputDir: filepath.Join(v.Workpath, m.identifier),
				Model:     m,
				Scenario:  v,
			}

			AssertNonMemCompleted(t, testingDetails)
			AssertNonMemCreatedOutputFiles(t, testingDetails)
			AssertContainsBBIScript(t, testingDetails)
			AssertNonMemOutputContainsParafile(t, testingDetails)
			AssertDataSourceIsHashedAndCorrect(t, testingDetails)
			AssertModelIsHashedAndCorrect(t, testingDetails)
		}
	}
}

func TestDefaultConfigLoaded(tt *testing.T) {
	t := wrapt.WrapT(tt)

	SkipIfNotEnabled(t, "LOCAL")
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()
	scenarios, err := InitializeScenarios([]string{
		"240",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 1)

	// Only work on the first one.
	scenario := scenarios[0]

	nonMemArguments := []string{
		"-d",
		"nonmem",
		"run",
		"local",
		"--nm_version",
		os.Getenv("NMVERSION"),
	}

	t.R.NoError(scenario.Prepare(ctx))

	for _, v := range scenario.models {
		out, _ := v.Execute(scenario, nonMemArguments...)
		nmd := NonMemTestingDetails{
			OutputDir: "",
			Model:     v,
			Output:    out,
		}

		AssertDefaultConfigLoaded(t, nmd)
	}
}

func TestSpecifiedConfigByAbsPathLoaded(tt *testing.T) {
	t := wrapt.WrapT(tt)

	// SkipIfNotEnabled("LOCAL",t)
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

	_, err := io.Copy(dest, source)
	t.R.NoError(err)

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()
	scenarios, err := InitializeScenarios([]string{
		"240",
		"acop",
		"ctl_test",
		"metrum_std",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 4)

	// Only work on the first one.
	scenario := scenarios[0]

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

	t.R.NoError(scenario.Prepare(ctx))

	for _, v := range scenario.models {
		out, _ := v.Execute(scenario, nonMemArguments...)
		nmd := NonMemTestingDetails{
			OutputDir: "",
			Model:     v,
			Output:    out,
		}

		AssertSpecifiedConfigLoaded(t, nmd, filepath.Join(ROOT_EXECUTION_DIR, "meow", "bbi.yaml"))
	}
}

func TestSpecifiedConfigByRelativePathLoaded(tt *testing.T) {
	t := wrapt.WrapT(tt)

	SkipIfNotEnabled(t, "LOCAL")
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

	_, err := io.Copy(dest, source)
	t.R.NoError(err)

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()
	scenarios, err := InitializeScenarios([]string{
		"240",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 1)

	// Only work on the first one.
	scenario := scenarios[0]

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

	t.R.NoError(scenario.Prepare(ctx))

	for _, v := range scenario.models {
		out, _ := v.Execute(scenario, nonMemArguments...)
		nmd := NonMemTestingDetails{
			OutputDir: "",
			Model:     v,
			Output:    out,
		}

		AssertSpecifiedConfigLoaded(t, nmd, filepath.Join(ROOT_EXECUTION_DIR, "meow", "bbi.yaml"))
	}
}

func SkipIfNotEnabled(t *wrapt.T, feature string) {
	if !FeatureEnabled(feature) {
		t.Skip()
	}
}
