package utils

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/spf13/afero"
)

func TestDirExists(t *testing.T) {
	type test struct {
		input    string
		expected bool
	}

	data := []test{
		{".", true},
		{"./", true},
		{"..", true},
		{"../", true},
		{"./..", true},
		{"./../", true},
		{os.TempDir(), true},
		{os.TempDir() + FilePathSeparator, true},
		{"/", true},
		{"/some-really-random-directory-name", false},
		{"/some/really/random/directory/name", false},
		{"./some-really-random-local-directory-name", false},
		{"./some/really/random/local/directory/name", false},
	}

	for i, d := range data {
		exists, _ := DirExists(filepath.FromSlash(d.input), new(afero.OsFs))
		if d.expected != exists {
			t.Errorf("Test %d failed. Expected %t got %t", i, d.expected, exists)
		}
	}
}

func TestIsDir(t *testing.T) {
	type test struct {
		input    string
		expected bool
	}
	data := []test{
		{"./", true},
		{"/", true},
		{"./this-directory-does-not-existi", false},
		{"/this-absolute-directory/does-not-exist", false},
	}

	for i, d := range data {

		exists, _ := IsDir(d.input, new(afero.OsFs))
		if d.expected != exists {
			t.Errorf("Test %d failed. Expected %t got %t", i, d.expected, exists)
		}
	}
}

func TestFileAndExt(t *testing.T) {
	type fileAndExt struct {
		file string
		ext  string
	}
	type test struct {
		input    string
		expected fileAndExt
	}
	data := []test{
		{"test.txt", fileAndExt{"test", ".txt"}},
		{"path/test.txt", fileAndExt{"test", ".txt"}},
		{"../relativepath/test.txt", fileAndExt{"test", ".txt"}},
		{"/absolutepath/test.txt", fileAndExt{"test", ".txt"}},
	}

	for i, d := range data {
		file, ext := FileAndExt(d.input)
		if file != d.expected.file {
			t.Errorf("Test %d failed. Expected %s got %s", i, d.expected.file, file)
		}
		if ext != d.expected.ext {
			t.Errorf("Test %d failed. Expected %s got %s", i, d.expected.ext, ext)
		}
	}
}
