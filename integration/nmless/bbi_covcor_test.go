package nmless

import (
	"context"
	"os"
	"path/filepath"
	"strconv"
	"testing"

	bi "github.com/metrumresearchgroup/bbi/integration"
	"github.com/metrumresearchgroup/bbi/utils"

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

	testId := "INT-COVCOR-001"
	for _, mod := range models {
		tt.Run(utils.AddTestId(mod.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			commandAndArgs := []string{
				"nonmem",
				"covcor",
				filepath.Join(SUMMARY_TEST_DIR, mod.name, mod.name),
			}

			output, err := bi.ExecuteCommand(context.Background(), "bbi", commandAndArgs...)

			t.R.NoError(err)
			t.R.NotEmpty(output)

			gtd := GoldenFileTestingDetails{
				outputString:   output,
				goldenFilePath: filepath.Join(SUMMARY_TEST_DIR, SUMMARY_GOLD_DIR, mod.name+".golden.covcor.json"),
			}

			if os.Getenv("UPDATE_COVCOR") == "true" {
				UpdateGoldenFile(t, gtd)
			}

			RequireOutputMatchesGoldenFile(t, gtd)
		})
	}
}

func TestCovCorErrors(tt *testing.T) {
	tests := []struct {
		args []string
		want string
	}{
		{
			args: []string{filepath.Join(SUMMARY_TEST_DIR, "12", "12")},
			want: noFilePresentError,
		},
		{
			args: []string{filepath.Join(SUMMARY_TEST_DIR, "iovmm", "iovmm")},
			want: noFilePresentError,
		},
		{
			args: []string{},
			want: "one positional argument",
		},
		{
			args: []string{"one", "two"},
			want: "one positional argument",
		},
	}

	for i, tc := range tests {
		tt.Run(strconv.Itoa(i), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			args := append([]string{"nonmem", "covcor"}, tc.args...)
			output, err := bi.ExecuteCommandNoErrorCheck(context.Background(), "bbi", args...)

			t.R.NotNil(err)
			t.A.Contains(output, tc.want)
		})
	}
}
