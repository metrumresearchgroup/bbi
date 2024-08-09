package bbitest

import (
	"context"
	"io"
	"os"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/wrapt"
)

func TestNMQUALExecutionSucceeds(tt *testing.T) {
	nmversion := os.Getenv("NMVERSION_NMQUAL")
	if nmversion == "" {
		tt.Skip("Testing for NMQUAL not enabled")
	}

	t := wrapt.WrapT(tt)

	scenario := InitializeScenario(t, "ctl_test")
	scenario.Prepare(t, context.Background())

	testId := "INT-NMQ-001"
	for _, m := range scenario.models {
		t.Run(utils.AddTestId(m.identifier, testId), func(tt *wrapt.T) {
			args := []string{
				"nonmem",
				"run",
				"--nm_version", nmversion,
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
			AssertNonMemCleanedUpFiles(t, nmd)
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
	nmversion := os.Getenv("NMVERSION_NMQUAL")
	if nmversion == "" {
		tt.Skip("Testing for NMQUAL not enabled")
	}

	t := wrapt.WrapT(tt)

	scenario := InitializeScenario(t, "acop")
	scenario.Prepare(t, context.Background())

	testId := "INT-NMQ-002"
	for _, m := range scenario.models {
		t.Run(utils.AddTestId(m.identifier, testId), func(t *wrapt.T) {
			args := []string{
				"nonmem",
				"run",
				"--nm_version", nmversion,
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
			AssertNonMemCleanedUpFiles(t, nmd)
			AssertScriptContainsAutologReference(t, nmd)
			AssertDataSourceIsHashedAndCorrect(t, nmd)
			AssertModelIsHashedAndCorrect(t, nmd)
		})
	}
}

func AssertScriptContainsAutologReference(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	scriptFile, _ := os.Open(filepath.Join(details.OutputDir, details.Model.identifier+".sh"))
	bytes, _ := io.ReadAll(scriptFile)
	err := scriptFile.Close()
	t.R.NoError(err)

	scriptFileContent := string(bytes)

	t.R.Contains(scriptFileContent, "autolog.pl")
}
