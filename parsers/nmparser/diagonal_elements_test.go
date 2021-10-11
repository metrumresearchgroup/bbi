package parser

import (
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/wrapt"
)

func TestGetDiagonalIndices(tt *testing.T) {
	tests := []struct {
		name     string
		input    []int
		expected []int
	}{
		{
			name:     "0",
			input:    []int{1, 2, 3},
			expected: []int{0, 2},
		},
		{
			name:     "1",
			input:    []int{1, 0, 2, 0, 0, 3},
			expected: []int{0, 2, 5},
		},
		{
			name:     "2",
			input:    []int{1, 1, 1, 1, 1, 1},
			expected: []int{0, 2, 5},
		},
		{
			name:     "3",
			input:    []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
			expected: []int{0, 2, 5, 9},
		},
		{
			name:     "4",
			input:    []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16},
			expected: []int{0, 2, 5, 9, 14},
		},
		// {
		//  name: "5",
		// 	input: []int{1, 1, 1, 2},
		// 	expected: []int{0, 2, 3},
		// },
	}
	testId := "UNIT-NMP-031"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			actual := GetDiagonalIndices(test.input)

			t.R.Equal(test.expected, actual)
		})
	}
}

func TestGetDiagonalIndices2(tt *testing.T) {
	tests := []struct {
		name     string
		input    int
		expected []int
	}{
		{
			name:     "0",
			input:    3,
			expected: []int{0, 2},
		},
		{
			name:     "1",
			input:    6,
			expected: []int{0, 2, 5},
		},
		{
			name:     "2",
			input:    10,
			expected: []int{0, 2, 5, 9},
		},
		{
			name:     "3",
			input:    15,
			expected: []int{0, 2, 5, 9, 14},
		},
		{
			name:     "4",
			input:    21,
			expected: []int{0, 2, 5, 9, 14, 20},
		},
		{
			name:     "5",
			input:    45,
			expected: []int{0, 2, 5, 9, 14, 20, 27, 35, 44},
		},
	}
	testId := "UNIT-NMP-032"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			var input []int
			for i := 1; i <= test.input; i++ {
				input = append(input, i)
			}

			actual := GetDiagonalIndices(input)

			t.R.Equal(test.expected, actual)
		})
	}
}

func TestGetBlockParameterNames(tt *testing.T) {
	tests := []struct {
		name     string
		function string
		length   int
		expected []string
	}{
		{
			name:     "3 omega",
			function: "OMEGA",
			length:   3,
			expected: []string{"OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)"},
		},
		{
			name:     "6 omega",
			function: "OMEGA",
			length:   6,
			expected: []string{"OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)"},
		},
		{
			name:     "10 omega",
			function: "OMEGA",
			length:   10,
			expected: []string{"OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)", "OMEGA(4,1)", "OMEGA(4,2)", "OMEGA(4,3)", "OMEGA(4,4)"},
		},
		{
			name:     "3 sigma",
			function: "SIGMA",
			length:   3,
			expected: []string{"SIGMA(1,1)", "SIGMA(2,1)", "SIGMA(2,2)"},
		},
	}
	testId := "UNIT-NMP-033"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got := GetBlockParameterNames(test.function, test.length)

			t.R.Equal(test.expected, got)
		})
	}
}
