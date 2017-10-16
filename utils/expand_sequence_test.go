package utils

import "testing"

func TestExpandSequence(t *testing.T) {
	type test struct {
		input    string
		expected []string
	}
	data := []test{
		{"run[001:002].mod", []string{"run001.mod", "run002.mod"}},
		{"run001.mod", []string{"run001.mod"}},
		{"run001", []string{"run001"}},
		{"run[001:002]", []string{"run001", "run002"}},
	}

	for i, d := range data {
		res, err := ExpandNameSequence(d.input)
		if err != nil {
			t.Error(err)
		}
		for j, r := range res {
			if d.expected[j] != r {
				t.Errorf("Test %d failed. Expected %s got %s", i, d.expected[j], r)
			}
		}
	}
}
