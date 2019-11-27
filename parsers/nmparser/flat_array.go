package parser

// MakeFlatArray creates a flat array from a matrix-style
// input of rows and columns
func MakeFlatArray(matrix [][]float64, length int) FlatArray {
	if length > len(matrix) {
		panic("error making FlatArray")
	}

	values := make([]float64, length*length)
	k := 0
	for i := 0; i < length; i++ {
		for j := 0; j < length; j++ {
			values[k] = matrix[i][j]
			k++
		}
	}
	return FlatArray{
		Values: values,
		Dim:    length,
	}
}
