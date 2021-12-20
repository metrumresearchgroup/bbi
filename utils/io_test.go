package utils

import (
	"fmt"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestReadParamsAndOutputFromExt(tt *testing.T) {
	// simply check that reading in the entire file vs only the values that should be taken
	// will change the number of rows present
	tests := []struct {
		input     string
		length    int
		extlength int
	}{
		{
			input:     "testdata/basic.ext",
			length:    13,
			extlength: 6,
		},
		{
			input:     "testdata/two_tables.ext",
			length:    17,
			extlength: 11,
		},
	}

	testId := "UNIT-UTL-008"
	for _, test := range tests {
		tt.Run(AddTestId(test.input, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			fullLines, _ := ReadLines(test.input)
			extLines, _ := ReadParamsAndOutputFromExt(test.input)
			t.A.Equal(test.length, len(fullLines), fmt.Sprintf("%s-full", test.input))
			t.A.Equal(test.extlength, len(extLines), fmt.Sprintf("%s-ext", test.input))
		})
	}
}

func TestHasZero(tt *testing.T) {
	tests := []struct {
		floats   []float64
		expected bool
		context  string
	}{
		{
			floats:   []float64{0, 0, 0, 0, 0, 0},
			expected: true,
			context:  "all zeroes",
		},
		{
			floats:   []float64{10, 10, 10, 10, 10, 10},
			expected: false,
			context:  "no zero",
		},
		{
			floats:   []float64{},
			expected: false,
			context:  "empty",
		},
		{
			floats:   []float64{0, 10, 10, 10, 10, 10},
			expected: true,
			context:  "first zero",
		},
		{
			floats:   []float64{10, 10, 10, 10, 10, 0},
			expected: true,
			context:  "last zero",
		},
		{
			floats:   []float64{10, 0, 10},
			expected: true,
			context:  "middle zero",
		},
	}

	testId := "UNIT-UTL-009"
	for _, test := range tests {
		tt.Run(AddTestId(test.context, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got := HasZero(test.floats)

			t.A.Equal(test.expected, got)
		})
	}
}
