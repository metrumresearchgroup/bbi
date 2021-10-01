package parser

import (
	"github.com/metrumresearchgroup/bbi/utils"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestCreateDiagonalBlock(tt *testing.T) {
	tests := []struct {
		name     string
		input    int
		expected []int
	}{
		{
			name:     "1-diagonal",
			input:    1,
			expected: []int{1},
		},
		{
			name:     "2-diagonal",
			input:    2,
			expected: []int{1, 0, 1},
		},
		{
			name:     "3-diagonal",
			input:    3,
			expected: []int{1, 0, 1, 0, 0, 1},
		},
	}
	testId := "UNIT-NMP-006"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got := createDiagonalBlock(test.input)

			t.R.Equal(test.expected, got)
		})
	}
}
