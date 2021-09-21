package bbitest

import (
	"context"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestNMQUALExecutionSucceeds(tt *testing.T) {
	if !FeatureEnabled("NMQUAL") {
		tt.Skip("Testing for NMQUAL not enabled")
	}

	t := wrapt.WrapT(tt)

	scenario := InitializeScenario(t, "ctl_test")
	scenario.Prepare(t, context.Background())

	for _, m := range scenario.models {
		t.Run(m.identifier, func(tt *wrapt.T) {
			args := []string{
				"nonmem",
				"run",
				"--nm_version",
				os.Getenv("NMVERSION"),
				"--nmqual=true",
				"local",
			}

			output, err := m.Execute(scenario, args...)

			nmd := NonMemTestingDetails{
				OutputDir: filepath.Join(scenario.Workpath, m.identifier),
				Model:     m,
				Output:    output,
				Scenario:  scenario,
			}

			t.R.NoError(err)
			AssertNonMemCompleted(t, nmd)
			AssertNonMemCreatedOutputFiles(t, nmd)
			AssertScriptContainsAutologReference(t, nmd)
			AssertDataSourceIsHashedAndCorrect(t, nmd)
			AssertModelIsHashedAndCorrect(t, nmd)
		})
	}
}

// This test targets a model with a .mod extension to make sure that
// After cloning and re-creating as a .ctl, that the application
// knows to look for what was originally there; the .mod file.
func TestHashingForNMQualWorksWithOriginalModFile(tt *testing.T) {
	if !FeatureEnabled("NMQUAL") {
		tt.Skip("Testing for NMQUAL not enabled")
	}

	t := wrapt.WrapT(tt)

	scenario := InitializeScenario(t, "240")
	scenario.Prepare(t, context.Background())

	for _, m := range scenario.models {
		t.Run(m.identifier, func(t *wrapt.T) {
			args := []string{
				"nonmem",
				"run",
				"--nm_version",
				os.Getenv("NMVERSION"),
				"--nmqual=true",
				"local",
			}

			output, err := m.Execute(scenario, args...)

			nmd := NonMemTestingDetails{
				OutputDir: filepath.Join(scenario.Workpath, m.identifier),
				Model:     m,
				Output:    output,
				Scenario:  scenario,
			}

			t.R.NoError(err)
			AssertNonMemCompleted(t, nmd)
			AssertNonMemCreatedOutputFiles(t, nmd)
			AssertScriptContainsAutologReference(t, nmd)
			AssertDataSourceIsHashedAndCorrect(t, nmd)
			AssertModelIsHashedAndCorrect(t, nmd)
		})
	}
}

func AssertScriptContainsAutologReference(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	scriptFile, _ := os.Open(filepath.Join(details.OutputDir, details.Model.identifier+".sh"))
	bytes, _ := ioutil.ReadAll(scriptFile)
	err := scriptFile.Close()
	t.R.NoError(err)

	scriptFileContent := string(bytes)

	t.R.Contains(scriptFileContent, "autolog.pl")
}
