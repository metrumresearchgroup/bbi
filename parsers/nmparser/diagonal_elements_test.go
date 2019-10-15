package parser

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetDiagonalIndices(t *testing.T) {
	tests := []struct {
		input    []int
		expected []int
	}{
		{
			[]int{1, 2, 3},
			[]int{0, 2},
		},
		{
			[]int{1, 0, 2, 0, 0, 3},
			[]int{0, 2, 5},
		},
		// {
		// 	[]int{1, 1, 1, 2},
		// 	[]int{0, 2, 3},
		// },
		{
			[]int{1, 1, 1, 1, 1, 1},
			[]int{0, 2, 5},
		},
		{
			[]int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
			[]int{0, 2, 5, 9},
		},
		{
			[]int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16},
			[]int{0, 2, 5, 9, 14},
		},
	}
	for _, tst := range tests {
		actual := GetDiagonalIndices(tst.input)
		assert.Equal(t, tst.expected, actual)
	}
}

func TestGetDiagonalIndices2(t *testing.T) {
	tests := []struct {
		input    int
		expected []int
	}{
		{
			3,
			[]int{0, 2},
		},
		{
			6,
			[]int{0, 2, 5},
		},
		{
			10,
			[]int{0, 2, 5, 9},
		},
		{
			15,
			[]int{0, 2, 5, 9, 14},
		},
		{
			21,
			[]int{0, 2, 5, 9, 14, 20},
		},
		{
			45,
			[]int{0, 2, 5, 9, 14, 20, 27, 35, 44},
		},
	}
	for _, tst := range tests {

		//	input := make([]int, tst.input)
		var input []int
		for i := 1; i <= tst.input; i++ {
			input = append(input, i)
		}

		actual := GetDiagonalIndices(input)
		assert.Equal(t, tst.expected, actual)
	}
}
