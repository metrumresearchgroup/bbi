package parser

import (
	"fmt"
	"math"
)

// lowerDiagonalLengthToDimension takes l which is
// the number of total elements in the lower diagonal
//  matrix, and calculates the dimension (aka number of
// diagonal elements) for the matrix.
// Additionally, this function can be used to pass
// a specific index from the array of lower diagonal
// elements (as l). In that case, it will return one
// of the following:
// * If the index is a diagonal element:
//   * The number of that diagonal element
//   * true
// * If the index is not a diagonal element:
//   * 0
//   * false
// For example, ther are 6 elements in the lower
// diagonal of a 3x3 matrix, the 1st, 3rd, and 6th
// being diagonals. Calling this function on 1-6
// would return the following:
// lowerDiagonalLengthToDimension(1) => 1, true
// lowerDiagonalLengthToDimension(2) => 0, false
// lowerDiagonalLengthToDimension(3) => 2, true
// lowerDiagonalLengthToDimension(4) => 0, false
// lowerDiagonalLengthToDimension(5) => 0, false
// lowerDiagonalLengthToDimension(6) => 3, true
func lowerDiagonalLengthToDimension(l int) (int, bool) {
	​
	// calculate the dimension
	dim := int(math.Floor(math.Sqrt(2.0 * float64(l))))
	​
	// if not equal going back the other way then it's not a diagonal
	check := (dim * (dim + 1)) / 2
	if l != check {
		return 0, false
	}
	​
	return dim, true
}

func IndexAndIsDiag(i int) (int, bool) {
	res, isdiag := lowerDiagonalLengthToDimension(i+1)
	return res, isdiag
}

// GetDiagonalIndices returns the indices of the diagonal elements
// given a []int that represents which matrix various elements exist in
// for example 1 0 2 0 0 3 would represent 3 diagonal elements
// from something like $OMEGA 0.04 0.04 0.04
// this function should return []int{0, 2, 5}
//
// for full block matrices will get something like 1 2 2 2
// $OMEGA 0.04
// $OMEGA BLOCK(2)
// 0.04
// 0.001 0.04
// and should return []int{0, 1, 3}
func GetDiagonalIndices(el []int) []int {
	if len(el) == 0 {
		return []int{}
	}
	var diagElements []int
	for index := range el {
		_, ok := IndexAndIsDiag(index)
		if ok {
			diagElements = append(diagElements, index)
		}
	}
	return diagElements
}

// GetBlockParameterNames builds a slice of parameter names
// len is the number of elements in a lower triangular matrix
// len is converted to the dimension of the full matrix
// dimension = n, of a nxn block matrix
func GetBlockParameterNames(name string, len int) []string {
	var ret []string
	dim, _ := lowerDiagonalLengthToDimension(len)
	for i := 1; i <= dim; i++ {
		for j := 1; j <= dim; j++ {
			if i >= j {
				ret = append(ret, fmt.Sprintf("%s(%d,%d)", name, i, j))
			}
		}
	}
	return ret
}
