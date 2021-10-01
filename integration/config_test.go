package bbitest

import (
	"context"
	"encoding/json"
	"github.com/metrumresearchgroup/bbi/utils"
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
	tests := []struct {
		name string
	}{
		{name: "240"},
		{name: "acop"},
		{name: "ctl_test"},
		{name: "metrum_std"},
	}

	testId := "INT-CFG-001"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			scenario := InitializeScenario(t, test.name)
			scenario.Prepare(t, context.Background())

			for _, model := range scenario.models {
				t.Run(model.identifier, func(t *wrapt.T) {
					args := []string{
						"nonmem",
						"run",
						"local",
						"--nm_version",
						os.Getenv("NMVERSION"),
					}

					output, err := model.Execute(scenario, args...)

					t.A.Nil(err)
					t.A.NotNil(output)

					nmd := NonMemTestingDetails{
						OutputDir: filepath.Join(scenario.Workpath, model.identifier),
						Model:     model,
						Output:    output,
					}

					AssertNonMemCompleted(t, nmd)
					AssertNonMemCreatedOutputFiles(t, nmd)
					AssertBBIConfigJSONCreated(t, nmd)
					AssertBBIConfigContainsSpecifiedNMVersion(t, nmd, os.Getenv("NMVERSION"))
				})
			}
		})
	}
}

func AssertBBIConfigContainsSpecifiedNMVersion(t *wrapt.T, details NonMemTestingDetails, nmVersion string) {
	t.Helper()

	configFile, _ := os.Open(filepath.Join(details.OutputDir, "bbi_config.json"))
	cbytes, _ := ioutil.ReadAll(configFile)
	configFile.Close() // Go ahead and close the file handle

	nm := cmd.NonMemModel{}

	t.R.NoError(json.Unmarshal(cbytes, &nm))

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

	scenario := InitializeScenario(t, "240")
	scenario.Prepare(t, context.Background())

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

	testId := "INT-CFG-002"
	for _, m := range scenario.models {
		t.Run(utils.AddTestId(m.identifier, testId), func(tt *wrapt.T) {
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
		})
	}
}
