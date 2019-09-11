package parser

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetDiagonalElements(t *testing.T) {
	tests := []struct {
		input    []int
		expected []int
	}{
		{
			[]int{1, 0, 2, 0, 0, 3},
			[]int{0, 2, 5},
		},
		{
			[]int{1, 1, 1, 2},
			[]int{0, 2, 3},
		},
		{
			[]int{1, 1, 1, 1, 1, 1},
			[]int{0, 2, 5},
		},
	}
	for _, tst := range tests {
		actual := GetDiagonalElements(tst.input)
		assert.Equal(t, tst.expected, actual)
	}
}
