package bbitest

import (
	"context"
	"os"
	"path/filepath"
	"regexp"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestCovCorHappyPath(tt *testing.T) {
	var models = []struct {
		name string
	}{
		{name: "acop"},
		{name: "example2_itsimp"},
		{name: "1001"},
	}

	for _, mod := range models {
		tt.Run(mod.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			commandAndArgs := []string{
				"nonmem",
				"covcor",
				filepath.Join(SUMMARY_TEST_DIR, mod.name, mod.name),
			}

			output, err := executeCommand(context.Background(), "bbi", commandAndArgs...)

			t.R.NoError(err)
			t.R.NotEmpty(output)

			gtd := GoldenFileTestingDetails{
				outputString:   output,
				goldenFilePath: filepath.Join(SUMMARY_TEST_DIR, SUMMARY_GOLD_DIR, mod.name+".golden.covcor.json"),
			}

			if os.Getenv(":q") == "true" {
				UpdateGoldenFile(t, gtd)
			}

			RequireOutputMatchesGoldenFile(t, gtd)
		})
	}
}

func TestCovCorErrors(tt *testing.T) {
	var models = []struct {
		name string
	}{
		{name: "12"},
		{name: "iovmm"},
	}

	rgx := regexp.MustCompile(noFilePresentError)

	for _, mod := range models {
		tt.Run(mod.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			commandAndArgs := []string{
				"nonmem",
				"covcor",
				filepath.Join(SUMMARY_TEST_DIR, mod.name, mod.name),
			}

			// try without flag and get error
			var output string
			output, err := executeCommandNoErrorCheck(context.Background(), "bbi", commandAndArgs...)
			t.R.Error(err)

			errorMatch := rgx.MatchString(output)
			t.R.True(errorMatch)
		})
	}
}
