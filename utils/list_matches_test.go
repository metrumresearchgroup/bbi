package utils

import "testing"

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
		res, _ := ListMatchesByRegex(d.input.Names, d.input.Regex)
		for j, expected := range res {
			if d.expected[j] != expected {
				t.Errorf("Test %d failed. Expected %s got %s", i, d.expected[j], expected)
			}
		}
	}
}
