package parser

// createDiagonalBlock creates a slice of ints that would form a diagonal matrix with value 1 for diagonal and 0 for off diagonal elements
func createDiagonalBlock(num int) []int {
	var iArr []int
	for i := 1; i <= num; i++ {
		for j := 1; j <= i; j++ {
			if j == i {
				iArr = append(iArr, 1)
			} else {
				iArr = append(iArr, 0)
			}
		}
	}
	return iArr
}

func AnyTrue(bools []bool) bool {
	for _, v := range bools {
		if v {
			return true
		}
	}
	return false
}
