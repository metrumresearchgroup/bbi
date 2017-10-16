package runner

import (
	"reflect"
	"testing"

	"github.com/spf13/afero"
)

func TestReadCopiedFiles(t *testing.T) {
	results := []struct {
		input  string
		output []CopiedFile
	}{
		{
			"testdata/run100",
			[]CopiedFile{
				CopiedFile{"catab100", 1},
				CopiedFile{"run100.lst", 3},
			},
		},
	}
	fs := afero.NewOsFs()
	for _, r := range results {
		dat, err := ReadCopiedFiles(fs, r.input)
		if err != nil {
			t.Error(err)
		}
		if !reflect.DeepEqual(dat, r.output) {
			t.Log("GOT: ", dat, " Expected: ", r.output)
			t.Fail()
		}
	}
}

func TestGetCopiedFilenames(t *testing.T) {
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
		if err != nil {
			t.Error(err)
		}
		for i, f := range dat {
			ef := r.output[i]
			if f != r.output[i] {
				t.Log("GOT: ", f, " Expected: ", ef)
				t.Fail()
			}
		}
	}
}
