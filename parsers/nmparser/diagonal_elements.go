package parser

import "fmt"

// diagPerLowerTriParams provides the number
// of diagonal elements given a lower triangle
// structure of parameter estimates
// For example given the structure
// 1
// 2 3
// there are 3 values and 2 diagonal elements
var lowerDiagonalLengthToDimension = map[int]int{
	1:    1,
	3:    2,
	6:    3,
	10:   4,
	15:   5,
	21:   6,
	28:   7,
	36:   8,
	45:   9,
	55:   10,
	66:   11,
	78:   12,
	91:   13,
	105:  14,
	120:  15,
	136:  16,
	153:  17,
	171:  18,
	190:  19,
	210:  20,
	231:  21,
	253:  22,
	276:  23,
	300:  24,
	325:  25,
	351:  26,
	378:  27,
	406:  28,
	435:  29,
	465:  30,
	496:  31,
	528:  32,
	561:  33,
	595:  34,
	630:  35,
	666:  36,
	703:  37,
	741:  38,
	780:  39,
	820:  40,
	861:  41,
	903:  42,
	946:  43,
	990:  44,
	1035: 45,
	1081: 46,
	1128: 47,
	1176: 48,
	1225: 49,
	1275: 50,
}
var diagonalIndices = map[int]int{
	0:    1,
	2:    2,
	5:    3,
	9:    4,
	14:   5,
	20:   6,
	27:   7,
	35:   8,
	44:   9,
	54:   10,
	65:   11,
	77:   12,
	90:   13,
	104:  14,
	119:  15,
	135:  16,
	152:  17,
	170:  18,
	189:  19,
	209:  20,
	230:  21,
	252:  22,
	275:  23,
	299:  24,
	324:  25,
	350:  26,
	377:  27,
	405:  28,
	434:  29,
	464:  30,
	495:  31,
	527:  32,
	560:  33,
	594:  34,
	629:  35,
	665:  36,
	702:  37,
	740:  38,
	779:  39,
	819:  40,
	860:  41,
	902:  42,
	945:  43,
	989:  44,
	1034: 45,
	1080: 46,
	1127: 47,
	1175: 48,
	1224: 49,
	1274: 50,
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
		_, ok := diagonalIndices[index]
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
	dim := lowerDiagonalLengthToDimension[len]
	for i := 1; i <= dim; i++ {
		for j := 1; j <= dim; j++ {
			if i >= j {
				ret = append(ret, fmt.Sprintf("%s(%d,%d)", name, i, j))
			}
		}
	}
	return ret
}
