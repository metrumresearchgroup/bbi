package parser

// FlatArray ... to be consumed by R as matrix(Values, nrow=Dim)
type FlatArray struct {
	Values []float64
	Dim    int
}

// MakeFlatArray ...
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
