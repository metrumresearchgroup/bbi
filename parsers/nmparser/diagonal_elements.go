package parser

import "fmt"

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
var diagonalIndices = map[int]bool{
	1:    true,
	3:    true,
	6:    true,
	10:   true,
	15:   true,
	21:   true,
	28:   true,
	36:   true,
	45:   true,
	55:   true,
	66:   true,
	78:   true,
	91:   true,
	105:  true,
	120:  true,
	136:  true,
	153:  true,
	171:  true,
	190:  true,
	210:  true,
	231:  true,
	253:  true,
	276:  true,
	300:  true,
	325:  true,
	351:  true,
	378:  true,
	406:  true,
	435:  true,
	465:  true,
	496:  true,
	528:  true,
	561:  true,
	595:  true,
	630:  true,
	666:  true,
	703:  true,
	741:  true,
	780:  true,
	820:  true,
	861:  true,
	903:  true,
	946:  true,
	990:  true,
	1035: true,
	1081: true,
	1128: true,
	1176: true,
	1225: true,
	1275: true,
	1326: true,
	1378: true,
	1431: true,
	1485: true,
	1540: true,
	1596: true,
	1653: true,
	1711: true,
	1770: true,
	1830: true,
	1891: true,
	1953: true,
	2016: true,
	2080: true,
	2145: true,
	2211: true,
	2278: true,
	2346: true,
	2415: true,
	2485: true,
	2556: true,
	2628: true,
	2701: true,
	2775: true,
	2850: true,
	2926: true,
	3003: true,
	3081: true,
	3160: true,
	3240: true,
	3321: true,
	3403: true,
	3486: true,
	3570: true,
	3655: true,
	3741: true,
	3828: true,
	3916: true,
	4005: true,
	4095: true,
	4186: true,
	4278: true,
	4371: true,
	4465: true,
	4560: true,
	4656: true,
	4753: true,
	4851: true,
	4950: true,
	5050: true,
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
		if diagonalIndices[index+1] {
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
