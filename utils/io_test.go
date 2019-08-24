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
