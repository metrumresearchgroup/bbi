package bbitest

import (
	"context"
	"os"
	"path/filepath"
	"regexp"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestBBIRecleanBasic(tt *testing.T) {
	t := wrapt.WrapT(tt)

	dir := t.TempDir()
	fdata := filepath.Join(dir, "FDATA")
	fdataCSV := filepath.Join(dir, "FDATA.csv")

	_ = os.WriteFile(fdata, []byte("fake"), 0644)
	t.A.FileExists(fdata)
	_ = os.WriteFile(fdataCSV, []byte("fake"), 0644)
	t.A.FileExists(fdataCSV)

	output, err := executeCommand(context.Background(),
		"bbi", "nonmem", "reclean", "--recleanLvl=1", "-v", dir)
	t.R.NoError(err)
	t.R.NotEmpty(output)
	t.A.NoFileExists(fdata)
	t.A.NoFileExists(fdataCSV)
}

func TestBBIRecleanError(tt *testing.T) {
	t := wrapt.WrapT(tt)

	output, err := executeCommandNoErrorCheck(context.Background(),
		"bbi", "nonmem", "reclean")

	t.R.NotNil(err)
	errorMatch, _ := regexp.MatchString("one positional", output)
	t.R.True(errorMatch)
	errorMatch, _ = regexp.MatchString("Usage", output)
	t.R.True(errorMatch)
}
