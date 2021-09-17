package utils

import (
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestExpandSequence(tt *testing.T) {
	tests := []struct {
		input    string
		expected []string
	}{
		{
			input:    "run[001:002].mod",
			expected: []string{"run001.mod", "run002.mod"},
		},
		{
			input:    "run001.mod",
			expected: []string{"run001.mod"},
		},
		{
			input:    "run001",
			expected: []string{"run001"},
		},
		{
			input:    "run[001:002]",
			expected: []string{"run001", "run002"},
		},
		{
			input:    "run[1:3]",
			expected: []string{"run1", "run2", "run3"},
		},
		{
			input:    "run_1_[1:3]",
			expected: []string{"run_1_1", "run_1_2", "run_1_3"},
		},
		{
			input:    "[1:10].ctl",
			expected: []string{"1.ctl", "2.ctl", "3.ctl", "4.ctl", "5.ctl", "6.ctl", "7.ctl", "8.ctl", "9.ctl", "10.ctl"},
		},
	}

	for _, test := range tests {
		tt.Run(test.input, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			res, err := ExpandNameSequence(test.input)

			t.R.NoError(err)
			t.R.Equal(test.expected, res)
		})
	}
}
