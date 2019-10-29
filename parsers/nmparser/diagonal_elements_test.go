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

func TestGetBlockParameterNames(t *testing.T) {

	tests := []struct {
		name     string
		length   int
		expected []string
	}{
		{
			"OMEGA",
			3,
			[]string{"OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)"},
		},
		{
			"OMEGA",
			6,
			[]string{"OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)"},
		},
		{
			"OMEGA",
			10,
			[]string{"OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)", "OMEGA(4,1)", "OMEGA(4,2)", "OMEGA(4,3)", "OMEGA(4,4)"},
		},
		{
			"SIGMA",
			3,
			[]string{"SIGMA(1,1)", "SIGMA(2,1)", "SIGMA(2,2)"},
		},
	}

	for _, tt := range tests {
		sa := GetBlockParameterNames(tt.name, tt.length)
		assert.Equal(t, tt.expected, sa)
		assert.Equal(t, tt.length, len(sa))
	}
}

// func TestDiagMap(t *testing.T) {
// 	for n := 1; n < 100; n++ {
// 		L := n * (n + 1) / 2
// 		fmt.Printf("%d : %d,  \n", L, n)
// 	}
// }
