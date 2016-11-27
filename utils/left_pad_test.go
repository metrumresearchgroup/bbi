package utils

import "testing"

func TestPadNum(t *testing.T) {
	type test struct {
		input    int
		expected string
	}
	data := []test{
		{1, "001"},
		{10, "010"},
		{100, "100"},
		{1000, "1000"},
	}

	for i, d := range data {
		res := PadNum(d.input, 3)
		if d.expected != res {
			t.Errorf("Test %d failed. Expected %s got %s", i, d.expected, res)
		}
	}
}
