package utils

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
	"github.com/spf13/afero"
)

func TestDirExists(tt *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		{
			name:  "current",
			input: ".",
			want:  true,
		},
		{
			name:  "blank",
			input: "./",
			want:  true,
		},
		{
			name:  "parent",
			input: "..",
			want:  true,
		},
		{
			name:  "parent slash",
			input: "../",
			want:  true,
		},
		{
			name:  "current slash parent",
			input: "./..",
			want:  true,
		},
		{
			name:  "current slash parent slash",
			input: "./../",
			want:  true,
		},
		{
			name:  "tempdir",
			input: os.TempDir(),
			want:  true,
		},
		{
			name:  "tempdir slash",
			input: os.TempDir() + FilePathSeparator,
			want:  true,
		},
		{
			name:  "slash",
			input: "/",
			want:  true,
		},
		{
			name:  "slash name",
			input: "/some-really-random-directory-name",
			want:  false,
		},
		{
			name:  "slashed names",
			input: "/some/really/random/directory/name",
			want:  false,
		},
		{
			name:  "dot slash name",
			input: "./some-really-random-local-directory-name",
			want:  false,
		},
		{
			name:  "dot slashed names",
			input: "./some/really/random/local/directory/name",
			want:  false,
		},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got, _ := DirExists(filepath.FromSlash(test.input), new(afero.OsFs))

			t.R.Equal(test.want, got)
		})
	}
}

func TestIsDir(tt *testing.T) {
	tests := []struct {
		name  string
		input string
		want  bool
	}{
		{
			name:  "dot slash",
			input: "./",
			want:  true,
		},
		{
			name:  "slash",
			input: "/",
			want:  true,
		},
		{
			name:  "dot slash non-existent",
			input: "./this-directory-does-not-existi",
			want:  false,
		},
		{
			name:  "slash non-existent slash",
			input: "/this-absolute-directory/does-not-exist",
			want:  false,
		},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got, _ := IsDir(test.input, new(afero.OsFs))

			t.R.Equal(test.want, got)
		})
	}
}

func TestFileAndExt(tt *testing.T) {
	type fileAndExt struct {
		file string
		ext  string
	}
	tests := []struct {
		input string
		want  fileAndExt
	}{
		{
			input: "test.txt",
			want:  fileAndExt{"test", ".txt"},
		},
		{
			input: "path/test.txt",
			want:  fileAndExt{"test", ".txt"},
		},
		{
			input: "../relativepath/test.txt",
			want:  fileAndExt{"test", ".txt"},
		},
		{
			input: "/absolutepath/test.txt",
			want:  fileAndExt{"test", ".txt"},
		},
	}

	for _, test := range tests {
		tt.Run(test.input, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			file, ext := FileAndExt(test.input)

			t.R.Equal(test.want.file, file)
			t.R.Equal(test.want.ext, ext)
		})
	}
}
