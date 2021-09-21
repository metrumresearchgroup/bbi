package bbitest

import (
	"fmt"
	"io/ioutil"

	"github.com/metrumresearchgroup/wrapt"
)

type GoldenFileTestingDetails struct {
	outputString   string
	goldenFilePath string
}

// check that string in outputString matches contents of file at goldenFilePath.
func RequireOutputMatchesGoldenFile(t *wrapt.T, details GoldenFileTestingDetails) {
	t.Helper()

	gb, err := ioutil.ReadFile(details.goldenFilePath)
	if err != nil {
		err = fmt.Errorf("failed reading %s: %w", details.goldenFilePath, err)
	}
	t.R.NoError(err)

	gold := string(gb)

	t.R.Equal(gold, details.outputString, "output does not match .golden file "+details.goldenFilePath)
}

// Write string in outputString to file at goldenFilePath.
// If file already exists, it will be overwritten.
// User can then use git diff to see what has been updated.
func UpdateGoldenFile(t *wrapt.T, details GoldenFileTestingDetails) {
	t.Helper()

	t.Logf("updating golden file %s", details.goldenFilePath)

	err := ioutil.WriteFile(details.goldenFilePath, []byte(details.outputString), 0644)
	if err != nil {
		err = fmt.Errorf("failed to update %s: %w", details.goldenFilePath, err)
	}
	t.R.NoError(err)
}
