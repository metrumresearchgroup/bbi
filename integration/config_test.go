package bbitest

import (
	"context"
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
	"gopkg.in/yaml.v2"

	"github.com/metrumresearchgroup/bbi/cmd"
	"github.com/metrumresearchgroup/bbi/configlib"
)

func TestBBIConfigJSONCreated(tt *testing.T) {
	t := wrapt.WrapT(tt)

	scenarios, err := InitializeScenarios([]string{
		"240",
		"acop",
		"ctl_test",
		"metrum_std",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 4)

	for _, v := range scenarios {
		v.Prepare(context.Background())

		for _, m := range v.models {
			args := []string{
				"nonmem",
				"run",
				"local",
				"--nm_version",
				os.Getenv("NMVERSION"),
			}

			output, err := m.Execute(v, args...)

			t.A.Nil(err)
			t.A.NotNil(output)

			nmd := NonMemTestingDetails{
				OutputDir: filepath.Join(v.Workpath, m.identifier),
				Model:     m,
				Output:    output,
			}

			AssertNonMemCompleted(t, nmd)
			AssertNonMemCreatedOutputFiles(t, nmd)
			AssertBBIConfigJSONCreated(t, nmd)
			AssertBBIConfigContainsSpecifiedNMVersion(t, nmd, os.Getenv("NMVERSION"))
		}
	}
}

func AssertBBIConfigContainsSpecifiedNMVersion(t *wrapt.T, details NonMemTestingDetails, nmVersion string) {
	t.Helper()

	configFile, _ := os.Open(filepath.Join(details.OutputDir, "bbi_config.json"))
	cbytes, _ := ioutil.ReadAll(configFile)
	configFile.Close() // Go ahead and close the file handle

	nm := cmd.NonMemModel{}

	json.Unmarshal(cbytes, &nm)

	t.A.NotNil(nm)
	t.A.NotEqual(nm, cmd.NonMemModel{})
	t.A.Equal(nm.Configuration.NMVersion, nmVersion)
}

func TestConfigValuesAreCorrectInWrittenFile(tt *testing.T) {
	t := wrapt.WrapT(tt)

	// Get BB and make sure we have the test data moved over.
	// Clean Slate
	// Pick a few critical configuration components such as
	/*
		--clean_level 3
		--copy_level 1
		--debug
		--parallel=true <- make sure it's present
		--mpi_exec_path
	*/

	scenarios, err := InitializeScenarios([]string{
		"240",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 1)

	scenario := scenarios[0]
	err = scenario.Prepare(context.Background())
	t.R.NoError(err)

	commandAndArgs := []string{
		"--debug=true", // Needs to be in debug mode to generate the expected output
		"nonmem",
		"run",
		"--clean_lvl",
		"3",
		"--copy_lvl",
		"1",
		"--parallel=true",
		"--mpi_exec_path",
		os.Getenv("MPIEXEC_PATH"),
		"local",
		"--nm_version",
		os.Getenv("NMVERSION"),
	}

	for _, m := range scenario.models {
		output, err := m.Execute(scenario, commandAndArgs...)

		t.A.Nil(err)
		t.A.NotEmpty(output)

		nmd := NonMemTestingDetails{
			OutputDir: filepath.Join(scenario.Workpath, m.identifier),
			Model:     m,
			Output:    output,
		}

		AssertNonMemCompleted(t, nmd)
		AssertNonMemCreatedOutputFiles(t, nmd)
		AssertNonMemOutputContainsParafile(t, nmd)

		// Now read the Config Lib
		configFile := filepath.Join(scenario.Workpath, m.identifier, "bbi.yaml")
		file, _ := os.Open(configFile)
		Config := configlib.Config{}
		bytes, _ := ioutil.ReadAll(file)
		err = yaml.Unmarshal(bytes, &Config)

		t.A.Nil(err)

		t.A.Equal(3, Config.CleanLvl)
		t.A.Equal(1, Config.CopyLvl)
		t.A.Equal(true, Config.Parallel)
		t.A.Equal(os.Getenv("NMVERSION"), Config.NMVersion)

		t.A.Equal(os.Getenv("MPIEXEC_PATH"), Config.MPIExecPath)
		t.A.Equal(false, Config.Overwrite)
	}
}
