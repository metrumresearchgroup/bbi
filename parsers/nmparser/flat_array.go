package parser

// FlatArray provides a slice of values meant to be coerced to a matrix
// of the dimensions Dim
// This allows easy coercion to matrices in languages like R
// using matrix(Values, nrow = Dim)
type FlatArray struct {
	Values []float64
	Dim    int
}

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
