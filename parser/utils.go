package parser

// createDiagonalBlockSlice creates a slice of ints that would form a diagonal matrix
func createDiagonalBlockSlice(num int) []int {
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
