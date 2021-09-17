package runner

import (
	"testing"

	"github.com/metrumresearchgroup/wrapt"
	"github.com/spf13/afero"
)

func TestReadCopiedFiles(tt *testing.T) {
	t := wrapt.WrapT(tt)

	results := []struct {
		input  string
		output []TargetedFile
	}{
		{
			"testdata/run100",
			[]TargetedFile{
				TargetedFile{"catab100", 1},
				TargetedFile{"run100.lst", 3},
			},
		},
	}
	fs := afero.NewOsFs()
	for _, r := range results {
		dat, err := ReadCopiedFiles(fs, r.input)

		t.R.NoError(err)
		t.R.Equal(r.output, dat)
	}
}

func TestGetCopiedFilenames(tt *testing.T) {
	t := wrapt.WrapT(tt)

	results := []struct {
		input  string
		output []string
	}{
		{
			"testdata/run100",
			[]string{
				"catab100",
				"run100.lst",
			},
		},
	}
	fs := afero.NewOsFs()
	for _, r := range results {
		dat, err := GetCopiedFilenames(fs, r.input)

		t.R.NoError(err)
		t.R.Equal(r.output, dat)
	}
}
