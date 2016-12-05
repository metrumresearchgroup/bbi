package utils

import "testing"

func TestListModelFiles(t *testing.T) {
	type test struct {
		input    []string
		expected []string
	}
	data := []test{
		{
			[]string{
				"run001.mod",
				"run002.lst",
				"run003.mod",
				"nota.run",
			},
			[]string{
				"run001.mod",
				"run003.mod",
			},
		},
	}

	for i, d := range data {
		res := ListFilesByExt(d.input, ".mod")
		for j, expected := range res {
			if d.expected[j] != expected {
				t.Errorf("Test %d failed. Expected %s got %s", i, d.expected[j], expected)
			}
		}
	}
}
