package cmd

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestFindNonMemBinaryWindows(tt *testing.T) {
	t := wrapt.WrapT(tt)
	dir := t.TempDir()
	rdir := filepath.Join(dir, "run")
	if err := os.Mkdir(rdir, 0777); err != nil {
		t.Fatal(err)
	}

	foo := filepath.Join(rdir, "foo.bat")
	if err := os.WriteFile(foo, []byte(""), 0666); err != nil {
		t.Fatal(err)
	}

	_, err := findNonMemBinaryWindows(dir)
	t.R.Contains(err.Error(), ".bat file not found")

	nmfe := filepath.Join(rdir, "nmfe23.bat")
	if err = os.WriteFile(nmfe, []byte(""), 0666); err != nil {
		t.Fatal(err)
	}

	binary, err := findNonMemBinaryWindows(dir)
	t.R.NoError(err)
	t.R.Equal(binary, "nmfe23.bat")
}
