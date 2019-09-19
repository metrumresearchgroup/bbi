package utils

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestReadExt(t *testing.T) {
	type test struct {
		input     string
		length    int
		extlength int
	}
	// simply check that reading in the entire file vs only the values that should be taken
	// will change the number of rows present
	data := []test{
		{
			"testdata/basic.ext",
			13,
			6,
		},
		{
			"testdata/two_tables.ext",
			17,
			11,
		},
	}

	for _, d := range data {
		fullLines, _ := ReadLines(d.input)
		extLines, _ := ReadParamsAndOutputFromExt(d.input)
		assert.Equal(t, d.length, len(fullLines), fmt.Sprintf("%s-full", d.input))
		assert.Equal(t, d.extlength, len(extLines), fmt.Sprintf("%s-ext", d.input))
	}
}

func TestHasZero(t *testing.T) {
	type test struct {
		floats   []float64
		expected bool
		context  string
	}
	tests := []test{
		{
			[]float64{0, 0, 0, 0, 0, 0},
			true,
			"all zeroes",
		},
		{
			[]float64{10, 10, 10, 10, 10, 10},
			false,
			"no zero",
		},
		{
			[]float64{},
			false,
			"empty",
		},
		{
			[]float64{0, 10, 10, 10, 10, 10},
			true,
			"first zero",
		},
		{
			[]float64{10, 10, 10, 10, 10, 0},
			true,
			"last zero",
		},
		{
			[]float64{10, 0, 10},
			true,
			"middle zero",
		},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.expected, HasZero(tt.floats), "failed: "+tt.context)
	}
}
