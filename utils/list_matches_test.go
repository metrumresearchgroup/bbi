package utils

import (
	"regexp"
	"testing"

	"github.com/gobwas/glob"
)

func TestListMatchesByRegex(t *testing.T) {
	type input struct {
		Names []string
		Regex string
	}
	type test struct {
		input    input
		expected []string
	}
	exData := []string{
		"run001.mod",
		"run002.lst",
		"run003.mod",
		"nota.run",
	}

	data := []test{
		{
			input{
				Names: exData,
				Regex: "run00..mod",
			},
			[]string{
				"run001.mod",
				"run003.mod",
			},
		},
		{
			input{
				Names: exData,
				Regex: "run$",
			},
			[]string{
				"nota.run",
			},
		},
		{
			input{
				Names: exData,
				Regex: "^run",
			},
			[]string{
				"run001.mod",
				"run002.lst",
				"run003.mod",
			},
		},
	}

	for i, d := range data {
		glb, err := regexp.Compile(d.input.Regex)
		if err != nil{
			t.Fatalf("error in regex")
		}
		res := ListMatchesByRegex(d.input.Names, glb)
		for j, expected := range res {
			if d.expected[j] != expected {
				t.Errorf("Test %d failed. Expected %s got %s", i, d.expected[j], expected)
			}
		}
	}
}

func TestListMatchesByGlob(t *testing.T) {
	type input struct {
		Names []string
		Glob  string
	}
	type test struct {
		input    input
		expected []string
	}
	exData := []string{
		"run001.mod",
		"run002.lst",
		"run003.mod",
		"nota.run",
	}

	data := []test{
		{
			input{
				Names: exData,
				Glob:  "run*.mod",
			},
			[]string{
				"run001.mod",
				"run003.mod",
			},
		},
		{
			input{
				Names: exData,
				Glob:  "*run",
			},
			[]string{
				"nota.run",
			},
		},
		{
			input{
				Names: exData,
				Glob:  "run*",
			},
			[]string{
				"run001.mod",
				"run002.lst",
				"run003.mod",
			},
		},
	}

	for i, d := range data {
		glb, err := glob.Compile(d.input.Glob)
		if err != nil {
			t.Fatalf("glob compile failed")
		}
		res := ListMatchesByGlob(d.input.Names, glb)
		for j, expected := range res {
			if d.expected[j] != expected {
				t.Errorf("Test %d failed. Expected %s got %s", i, d.expected[j], expected)
			}
		}
	}
}
