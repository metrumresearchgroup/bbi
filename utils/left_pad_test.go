package utils

import (
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestPadNum(tt *testing.T) {
	tests := []struct {
		name     string
		input    int
		expected string
	}{
		{
			name:     "001",
			input:    1,
			expected: "001",
		},
		{
			name:     "010",
			input:    10,
			expected: "010",
		},
		{
			name:     "100",
			input:    100,
			expected: "100",
		},
		{
			name:     "1000",
			input:    1000,
			expected: "1000",
		},
	}
	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got := PadNum(test.input, 3)

			t.R.Equal(test.expected, got)
		})
	}
}
