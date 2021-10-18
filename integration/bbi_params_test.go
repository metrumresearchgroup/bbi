package bbitest

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"
	"github.com/metrumresearchgroup/wrapt"
)

var testConfigsParams = []testConfig{
	{
		"--json",
		".json",
	},
	{
		"",
		".csv",
	},
}

func TestParamsSingleModel(tt *testing.T) {
	mod := "example2_bayes"
	testId := "INT-PARAM-001"
	for _, tc := range testConfigsParams {
		tt.Run(utils.AddTestId(mod+tc.bbiOption, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			commandAndArgs := []string{
				"nonmem",
				"params",
				filepath.Join(SUMMARY_TEST_DIR, mod),
			}

			if tc.bbiOption != "" {
				commandAndArgs = append(commandAndArgs, tc.bbiOption)
			}

			output, err := executeCommand(context.Background(), "bbi", commandAndArgs...)

			t.R.NoError(err)
			t.R.NotEmpty(output)

			gtd := GoldenFileTestingDetails{
				outputString:   output,
				goldenFilePath: filepath.Join(SUMMARY_TEST_DIR, SUMMARY_GOLD_DIR, mod+".golden.params"+tc.goldenExt),
			}

			if os.Getenv("UPDATE_PARAMS") == "true" {
				UpdateGoldenFile(t, gtd)
			}

			RequireOutputMatchesGoldenFile(t, gtd)
		})
	}
}

func TestParamsDir(tt *testing.T) {
	testId := "INT-PARAM-002"
	for _, tc := range testConfigsParams {
		tt.Run(utils.AddTestId(tc.bbiOption, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			commandAndArgs := []string{
				"nonmem",
				"params",
				"--dir",
				SUMMARY_TEST_DIR,
			}

			if tc.bbiOption != "" {
				commandAndArgs = append(commandAndArgs, tc.bbiOption)
			}

			output, err := executeCommand(context.Background(), "bbi", commandAndArgs...)

			t.R.NoError(err)
			t.R.NotEmpty(output)

			gtd := GoldenFileTestingDetails{
				outputString:   output,
				goldenFilePath: filepath.Join(SUMMARY_TEST_DIR, SUMMARY_GOLD_DIR, "dir_bbi_summary.golden.params"+tc.goldenExt),
			}

			if os.Getenv("UPDATE_PARAMS") == "true" {
				UpdateGoldenFile(t, gtd)
			}

			RequireOutputMatchesGoldenFile(t, gtd)
		})
	}
}
