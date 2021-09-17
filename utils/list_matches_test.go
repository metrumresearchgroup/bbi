package utils

import (
	"regexp"
	"testing"

	"github.com/gobwas/glob"
	"github.com/metrumresearchgroup/wrapt"
)

func TestListMatchesByRegex(tt *testing.T) {
	type input struct {
		Names []string
		Regex string
	}

	exData := []string{
		"run001.mod",
		"run002.lst",
		"run003.mod",
		"nota.run",
	}

	tests := []struct {
		input    input
		expected []string
	}{
		{
			input: input{
				Names: exData,
				Regex: "run00..mod",
			},
			expected: []string{
				"run001.mod",
				"run003.mod",
			},
		},
		{
			input: input{
				Names: exData,
				Regex: "run$",
			},
			expected: []string{
				"nota.run",
			},
		},
		{
			input: input{
				Names: exData,
				Regex: "^run",
			},
			expected: []string{
				"run001.mod",
				"run002.lst",
				"run003.mod",
			},
		},
	}

	for _, test := range tests {
		tt.Run(test.input.Regex, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			glb, err := regexp.Compile(test.input.Regex)
			t.R.NoError(err)

			res := ListMatchesByRegex(test.input.Names, glb)

			t.R.Equal(test.expected, res)
		})
	}
}

func TestListMatchesByGlob(tt *testing.T) {
	type input struct {
		Names []string
		Glob  string
	}
	exData := []string{
		"run001.mod",
		"run002.lst",
		"run003.mod",
		"nota.run",
	}

	tests := []struct {
		input    input
		expected []string
	}{
		{
			input: input{
				Names: exData,
				Glob:  "run*.mod",
			},
			expected: []string{
				"run001.mod",
				"run003.mod",
			},
		},
		{
			input: input{
				Names: exData,
				Glob:  "*run",
			},
			expected: []string{
				"nota.run",
			},
		},
		{
			input: input{
				Names: exData,
				Glob:  "run*",
			},
			expected: []string{
				"run001.mod",
				"run002.lst",
				"run003.mod",
			},
		},
	}

	for _, test := range tests {
		tt.Run(test.input.Glob, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			glb, err := glob.Compile(test.input.Glob)
			t.R.NoError(err)

			got := ListMatchesByGlob(test.input.Names, glb)

			t.R.Equal(test.expected, got)
		})
	}
}
