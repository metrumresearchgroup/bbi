package bbitest

import (
	"context"
	"io/ioutil"
	"path/filepath"
	"regexp"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestBBIRecleanBasic(tt *testing.T) {
	t := wrapt.WrapT(tt)

	dir := t.TempDir()
	fdata := filepath.Join(dir, "FDATA")
	_ = ioutil.WriteFile(fdata, []byte("fake"), 0644)
	t.A.FileExists(fdata)

	output, err := executeCommand(context.Background(),
		"bbi", "nonmem", "reclean", "--recleanLvl=1", "-v", dir)
	t.R.NoError(err)
	t.R.NotEmpty(output)
	t.A.NoFileExists(fdata)
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
