package bbitest

import (
	"context"
	"io/ioutil"
	"path/filepath"
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
