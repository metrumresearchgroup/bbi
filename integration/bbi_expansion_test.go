package bbitest

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/wrapt"
)

// Test that expansion works with 001-005 etc.
func TestBBIExpandsWithoutPrefix(tt *testing.T) {
	testId := "INT-EXP-001"
	tt.Run(utils.AddTestId("", testId), func(tt *testing.T) {
		//	lines :=`DEBU[0000] expanded models: [240/001.mod 240/002.mod 240/003.mod 240/004.mod 240/005.mod 240/006.mod 240/007.mod 240/008.mod 240/009.mod]
		// INFO[0000] A total of 9 models have completed the initial preparation phase`
		t := wrapt.WrapT(tt)

		scenario := InitializeScenario(t, "bbi_expansion")
		scenario.Prepare(t, context.Background())

		targets := `10[1:5].ctl`

		commandAndArgs := []string{
			"-d", // Needs to be in debug mode to generate the expected output
			"--threads",
			"2",
			"nonmem",
			"run",
			"local",
			"--nm_version",
			os.Getenv("NMVERSION"),
			filepath.Join(scenario.Workpath, "model", targets),
		}

		output, err := executeCommand(context.Background(), "bbi", commandAndArgs...)

		t.R.NoError(err)
		t.R.NotEmpty(output)

		modelsLine, _ := findOutputLine(strings.Split(output, "\n"))
		modelsLine = strings.TrimSuffix(modelsLine, "\n")
		expandedModels := outputLineToModels(modelsLine)

		// Verify that we expanded to five models
		t.R.Len(expandedModels, 5)

		// Verify nonmem completed for all five
		for _, m := range expandedModels {
			tt.Run(utils.AddTestId(m, testId), func(tt *testing.T) {
				t := wrapt.WrapT(tt)

				file := filepath.Base(m)
				extension := filepath.Ext(file)
				identifier := strings.Replace(file, extension, "", 1)
				outputDir := filepath.Join(scenario.Workpath, "model", identifier)

				internalModel := Model{
					identifier: identifier,
					filename:   file,
					extension:  extension,
					path:       outputDir,
				}

				nmd := NonMemTestingDetails{
					OutputDir: internalModel.path,
					Model:     internalModel,
					Output:    output,
					Scenario:  scenario,
				}

				AssertNonMemCompleted(t, nmd)
				AssertNonMemCreatedOutputFiles(t, nmd)
				AssertNonMemCleanedUpFiles(t, nmd)
			})
		}
	})
}

// Test that expansion works with 001-005 etc.
func TestBBIExpandsWithPrefix(tt *testing.T) {
	testId := "INT-EXP-002"
	tt.Run(utils.AddTestId("", testId), func(tt *testing.T) {
		//	lines :=`DEBU[0000] expanded models: [240/001.mod 240/002.mod 240/003.mod 240/004.mod 240/005.mod 240/006.mod 240/007.mod 240/008.mod 240/009.mod]
		// INFO[0000] A total of 9 models have completed the initial preparation phase`
		t := wrapt.WrapT(tt)

		scenario := InitializeScenario(t, "bbi_expansion")
		scenario.Prepare(t, context.Background())

		targets := `bbi_mainrun_10[1:3].ctl`

		commandAndArgs := []string{
			"-d", // Needs to be in debug mode to generate the expected output
			"--threads",
			"2",
			"nonmem",
			"run",
			"local",
			"--nm_version",
			os.Getenv("NMVERSION"),
			filepath.Join(scenario.Workpath, "model", targets),
		}

		output, err := executeCommand(context.Background(), "bbi", commandAndArgs...)

		t.R.NoError(err)
		t.R.NotEmpty(output)

		modelsLine, _ := findOutputLine(strings.Split(output, "\n"))
		modelsLine = strings.TrimSuffix(modelsLine, "\n")
		expandedModels := outputLineToModels(modelsLine)

		// Verify that we expanded to three models
		t.R.Len(expandedModels, 3)

		// Verify nonmem completed for all five
		for _, m := range expandedModels {
			t.Run(utils.AddTestId(m, testId), func(t *wrapt.T) {
				file := filepath.Base(m)
				extension := filepath.Ext(file)
				identifier := strings.Replace(file, extension, "", 1)
				outputDir := filepath.Join(scenario.Workpath, "model", identifier)

				internalModel := Model{
					identifier: identifier,
					filename:   file,
					extension:  extension,
					path:       outputDir,
				}

				nmd := NonMemTestingDetails{
					OutputDir: internalModel.path,
					Model:     internalModel,
					Output:    output,
				}

				AssertNonMemCompleted(t, nmd)
				AssertNonMemCreatedOutputFiles(t, nmd)
				AssertNonMemCleanedUpFiles(t, nmd)
			})
		}
	})
}

// Test that expansion works with 001-005 etc.
func TestBBIExpandsWithPrefixToPartialMatch(tt *testing.T) {
	tests := []struct {
		name string
	}{
		{name: "bbi_expansion"},
	}
	testId := "INT-EXP-003"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			scenario := InitializeScenario(t, test.name)
			scenario.Prepare(t, context.Background())

			targets := `bbi_mainrun_10[2:3].ctl`

			commandAndArgs := []string{
				"-d", // Needs to be in debug mode to generate the expected output
				"--threads",
				"2",
				"nonmem",
				"run",
				"local",
				"--nm_version",
				os.Getenv("NMVERSION"),
				filepath.Join(scenario.Workpath, "model", targets),
			}

			output, err := executeCommand(context.Background(), "bbi", commandAndArgs...)

			t.R.NoError(err)
			t.R.NotEmpty(output)

			modelsLine, _ := findOutputLine(strings.Split(output, "\n"))
			modelsLine = strings.TrimSuffix(modelsLine, "\n")
			expandedModels := outputLineToModels(modelsLine)

			// Verify that we expanded to three models
			t.R.Len(expandedModels, 2)

			// Verify nonmem completed for all five
			for _, m := range expandedModels {
				t.Run(m, func(tt *wrapt.T) {
					file := filepath.Base(m)
					extension := filepath.Ext(file)
					identifier := strings.Replace(file, extension, "", 1)
					outputDir := filepath.Join(scenario.Workpath, "model", identifier)

					internalModel := Model{
						identifier: identifier,
						filename:   file,
						extension:  extension,
						path:       outputDir,
					}

					nmd := NonMemTestingDetails{
						OutputDir: internalModel.path,
						Model:     internalModel,
						Output:    output,
					}

					AssertNonMemCompleted(t, nmd)
					AssertNonMemCreatedOutputFiles(t, nmd)
					AssertNonMemCleanedUpFiles(t, nmd)
				})
			}
		})
	}
}

func outputLineToModels(expansionLine string) []string {
	slfields := strings.Split(expansionLine, ":")
	arrayComponent := slfields[len(slfields)-1]
	arrayComponent = strings.TrimSpace(arrayComponent)
	primaryPieces := strings.Split(arrayComponent, "[")[1]
	secondaryPieces := strings.Split(primaryPieces, "]")[0]
	models := strings.Fields(secondaryPieces)

	return models
}

func findOutputLine(outputLines []string) (string, error) {
	for _, v := range outputLines {
		if strings.Contains(v, "expanded models:") {
			return v, nil
		}
	}

	return "", errors.New("No matching line of text could be found in the provided output")
}
