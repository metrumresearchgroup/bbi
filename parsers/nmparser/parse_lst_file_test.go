package parser

import (
	"math"
	"math/rand"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestParseOBJV(tt *testing.T) {
	t := wrapt.WrapT(tt)

	var ofvDetails = []OfvDetails{NewOfvDetails("dummy")}
	var expected = []OfvDetails{NewOfvDetails("dummy")}

	ofvDetails = parseOFV("OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT: 821.705", ofvDetails)
	expected[0].OFVNoConstant = 821.705
	t.A.Equal(expected, ofvDetails)

	ofvDetails = parseOFV("OBJECTIVE FUNCTION VALUE WITH CONSTANT:       1639.561", ofvDetails)
	expected[0].OFVWithConstant = 1639.561
	t.A.Equal(expected, ofvDetails)

	ofvDetails = parseOFV("N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    817.855", ofvDetails)
	expected[0].ConstantToOFV = 817.855
	t.A.Equal(expected, ofvDetails)

	ofvDetails = parseOFV("#OBJV:********************************************    -7913.528       **************************************************", ofvDetails)
	expected[0].OFVNoConstant = -7913.528
	t.A.Equal(expected, ofvDetails)
}

func TestParTestParseOBJV2(tt *testing.T) {
	t := wrapt.WrapT(tt)

	var lines = []string{
		"#METH: dummy",
		"OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT: 821.705",
		"OBJECTIVE FUNCTION VALUE WITH CONSTANT:       1639.561",
		"N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    817.855",
		"#OBJV:********************************************    -7913.528       **************************************************",
	}
	expected := []OfvDetails{
		{
			EstMethod:       "dummy",
			ConstantToOFV:   817.855,
			OFVWithConstant: 1639.561,
			OFVNoConstant:   -7913.528,
		},
	}

	lstData := ParseLstEstimationFile(lines)
	t.A.Equal(lstData.OFV, expected)
}

func TestParTestParseOBJV3(tt *testing.T) {
	t := wrapt.WrapT(tt)

	var lines = []string{
		"#METH: dummy",
		"#OBJV:********************************************    -7913.528       **************************************************",
		"OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT: 821.705",
		"OBJECTIVE FUNCTION VALUE WITH CONSTANT:       1639.561",
		"N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    817.855",
	}
	expected := []OfvDetails{
		{
			EstMethod:       "dummy",
			ConstantToOFV:   817.855,
			OFVWithConstant: 1639.561,
			OFVNoConstant:   821.705,
		},
	}
	lstData := ParseLstEstimationFile(lines)
	t.A.Equal(expected, lstData.OFV)
}


func TestParseGradient(tt *testing.T) {
	var tests = []struct {
		lines                []string
		hasZeroFinalGradient bool
		context              string
	}{
		{
			lines: []string{
				"GRADIENT:  -3.8561E+01  4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   8.7727E+01  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   4.8447E+01  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   1.0154E-02  1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroFinalGradient: false,
			context:              "no zero gradient",
		},
		{
			lines: []string{
				"GRADIENT:   0           4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   8.7727E+01  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   4.8447E+01  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   1.0154E-02  1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroFinalGradient: false,
			context:              "zero gradient",
		},
		{
			lines: []string{
				"GRADIENT:  -3.8561E+01  4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   8.7727E+01  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   4.8447E+01  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   0           1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroFinalGradient: true,
			context:              "zero final gradient",
		},
		{
			lines: []string{
				"GRADIENT:   0  4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   0  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   0  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   0  1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroFinalGradient: true,
			context:              "zero gradients and zero final gradient",
		},
	}

	for _, test := range tests {
		tt.Run(test.context, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			noZeroFinal := parseGradient(test.lines)
			t.A.Equal(test.hasZeroFinalGradient, noZeroFinal, "Fail hasZeroFinalGradient: "+test.context)
		})
	}
}

func TestConditionNumber(tt *testing.T) {
	var tests = []struct {
		lines                []string
		n                    int
		conditionNumber      float64
		largeConditionNumber bool
		context              string
	}{
		{
			lines: []string{
				"************************************************************************************************************************",
				"********************                                                                                ********************",
				"********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************",
				"********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************",
				"********************                                                                                ********************",
				"************************************************************************************************************************",
				"                                                                                                                        ",
				"                                                                                                                        ",
				"            1         2         3         4         5         6         7         8         9                           ",
				"                                                                                                                        ",
				"        2.53E-01  4.29E-01  6.19E-01  7.76E-01  8.79E-01  9.30E-01  1.19E+00  1.31E+00  2.61E+00                        ",
				"                                                                                                                        ",
				" Elapsed finaloutput time in seconds:     0.16                                                                           ",
			},
			n:               3,
			conditionNumber: 10.316,
			context:         "not large",
		},
		{
			lines: []string{
				"************************************************************************************************************************",
				"********************                                                                                ********************",
				"********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************",
				"********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************",
				"********************                                                                                ********************",
				"************************************************************************************************************************",
				"                                                                                                                        ",
				"                                                                                                                        ",
				"            1         2         3         4         5         6         7         8         9                           ",
				"                                                                                                                        ",
				"        2.53E-01  4.29E-01  6.19E-01  7.76E-01  8.79E-01  9.30E-01  1.19E+00  1.31E+00  2.61E+03                        ",
				"                                                                                                                        ",
				" Elapsed finaloutput time in seconds:     0.16                                                                           ",
			},
			n:               3,
			conditionNumber: 10316.206,
			context:         "large",
		},
		// {
		// 	lines:                []string{"", ""},
		// 	n:                    0,
		// 	largeConditionNumber: Undefined,
		// 	context:              "not set",
		// },

		{
			lines: []string{
				"                                                                                                                                   ",
				" ************************************************************************************************************************          ",
				" ********************                                                                                ********************          ",
				" ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************          ",
				" ********************                   EIGENVALUES OF COR MATRIX OF ESTIMATE (RSR)                  ********************          ",
				" ********************                                                                                ********************          ",
				" ************************************************************************************************************************          ",
				"                                                                                                                                   ",
				"                                                                                                                                   ",
				"             1         2         3         4         5         6         7         8         9        10        11        12       ",
				"             13        14        15        16        17        18        19                                                        ",
				"                                                                                                                                   ",
				"         7.19E-11  1.01E-08  1.87E-07  9.06E-07  1.63E-06  3.50E-06  2.87E-05  3.76E-04  1.14E-03  1.47E-02  1.95E-02  2.31E-02    ",
				"          3.21E-02  7.21E-02  3.80E-01  5.27E-01  1.11E+00  1.19E+00  1.56E+01                                                     ",
				"                                                                                                                                   ",
				"                                                                                                                                   ",
				"Elapsed postprocess time in seconds:     7.30                                                                                      ",
			},
			n:               5,
			conditionNumber: 2.16968011126565e+11,
			context:         "two lines",
		},
	}

	for _, test := range tests {
		tt.Run(test.context, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			conditionNumber := mustCalculateConditionNumber(test.lines, test.n)

			// compare to three decimal places
			t.A.Equal(test.conditionNumber, math.Round(conditionNumber*1000)/1000, "Fail :"+test.context)
		})
	}
}

func TestSetCorrelationsOk(tt *testing.T) {
	var tests = []struct {
		lines          []string
		n              int
		correlationsOk bool
		context        string
	}{
		{
			lines:          []string{"", ""},
			n:              0,
			correlationsOk: false,
			context:        "empty lines, not OK",
		},
		{
			lines: []string{
				"************************************************************************************************************************",
				"********************                                                                                ********************",
				"********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************",
				"********************                          CORRELATION MATRIX OF ESTIMATE                        ********************",
				"********************                                                                                ********************",
				"************************************************************************************************************************",
				"                                                                                                                        ",
				"                                                                                                                        ",
				"		   TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      TH 9      OM11      OM12      OM13",
				"			OM22      OM23      OM33      SG11",
				"",
				" TH 1",
				"+        6.09E-01",
				"",
				" TH 2",
				"+       -7.99E-02  4.41E+00",
				"",
				" TH 3",
				"+       -1.06E-01 -3.44E-01  2.33E+00",
				"",
				" TH 4",
				"+       -3.91E-02 -4.46E-01  6.10E-01  1.01E+00",
				"",
				" TH 5",
				"+        8.05E-02  3.89E-01 -5.73E-01 -7.30E-01  2.13E-02",
				"",
				" TH 6",
				"+       ......... ......... ......... ......... ......... .........",
				"",
				" TH 7",
				"+       ......... ......... ......... ......... ......... ......... .........",
				"",
				" TH 8",
				"+       ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" TH 9",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM11",
				"+        6.62E-02 -5.65E-02 -2.33E-02  7.85E-02  3.10E-02 ......... ......... ......... .........  9.59E-03",
				"",
				" OM12",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM13",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM22",
				"+       -1.25E-01  1.41E-01  3.34E-02  2.03E-02  3.38E-02 ......... ......... ......... .........  1.05E-01 ......... .........",
				"         3.53E-03",
				"",
				" OM23",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"         ......... .........",
				"",
				" OM33",
				"+        9.00E-02 -2.64E-02  4.10E-02 -2.81E-02  6.29E-02 ......... ......... ......... .........  8.87E-02 ......... .........",
				"         -4.90E-02 .........  1.86E-03",
				"",
				" SG11",
				"+       -3.78E-02  8.42E-02 -1.56E-01 -1.15E-01  9.65E-02 ......... ......... ......... ......... -1.93E-01 ......... .........",
				"         9.53E-03 ......... -7.70E-02  7.34E-05",
				"",
				"",
				"1",
			},
			n:              3,
			correlationsOk: true,
			context:        "OK",
		},
		{
			lines: []string{
				"************************************************************************************************************************",
				"********************                                                                                ********************",
				"********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************",
				"********************                          CORRELATION MATRIX OF ESTIMATE                        ********************",
				"********************                                                                                ********************",
				"************************************************************************************************************************",
				"                                                                                                                        ",
				"                                                                                                                        ",
				"		   TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      TH 9      OM11      OM12      OM13",
				"			OM22      OM23      OM33      SG11",
				"",
				" TH 1",
				"+        6.09E-01",
				"",
				" TH 2",
				"+       -9.99E-01  4.41E+00",
				"",
				" TH 3",
				"+       -1.06E-01 -3.44E-01  2.33E+00",
				"",
				" TH 4",
				"+       -3.91E-02 -4.46E-01  6.10E-01  1.01E+00",
				"",
				" TH 5",
				"+        8.05E-02  3.89E-01 -5.73E-01 -7.30E-01  2.13E-02",
				"",
				" TH 6",
				"+       ......... ......... ......... ......... ......... .........",
				"",
				" TH 7",
				"+       ......... ......... ......... ......... ......... ......... .........",
				"",
				" TH 8",
				"+       ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" TH 9",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM11",
				"+        6.62E-02 -5.65E-02 -2.33E-02  7.85E-02  3.10E-02 ......... ......... ......... .........  9.59E-03",
				"",
				" OM12",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM13",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM22",
				"+       -1.25E-01  1.41E-01  3.34E-02  2.03E-02  3.38E-02 ......... ......... ......... .........  1.05E-01 ......... .........",
				"         3.53E-03",
				"",
				" OM23",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"         ......... .........",
				"",
				" OM33",
				"+        9.00E-02 -2.64E-02  4.10E-02 -2.81E-02  6.29E-02 ......... ......... ......... .........  8.87E-02 ......... .........",
				"         -4.90E-02 .........  1.86E-03",
				"",
				" SG11",
				"+       -3.78E-02  8.42E-02 -1.56E-01 -1.15E-01  9.65E-02 ......... ......... ......... ......... -1.93E-01 ......... .........",
				"         9.53E-03 ......... -7.70E-02  7.34E-05",
				"",
			},
			n:              3,
			correlationsOk: false,
			context:        "Not Ok, 2nd row",
		},

		{
			lines: []string{
				"************************************************************************************************************************",
				"********************                                                                                ********************",
				"********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************",
				"********************                          CORRELATION MATRIX OF ESTIMATE                        ********************",
				"********************                                                                                ********************",
				"************************************************************************************************************************",
				"                                                                                                                        ",
				"                                                                                                                        ",
				"		   TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      TH 9      OM11      OM12      OM13",
				"			OM22      OM23      OM33      SG11",
				"",
				" TH 1",
				"+        6.09E-01",
				"",
				" TH 2",
				"+       -7.99E-02  4.41E+0",
				"",
				" TH 3",
				"+       -1.06E-01 -3.44E-01  2.33E+00",
				"",
				" TH 4",
				"+       -3.91E-02 -4.46E-01  6.10E-01  1.01E+00",
				"",
				" TH 5",
				"+        8.05E-02  3.89E-01 -5.73E-01 -7.30E-01  2.13E-02",
				"",
				" TH 6",
				"+       ......... ......... ......... ......... ......... .........",
				"",
				" TH 7",
				"+       ......... ......... ......... ......... ......... ......... .........",
				"",
				" TH 8",
				"+       ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" TH 9",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM11",
				"+        6.62E-02 -5.65E-02 -2.33E-02  7.85E-02  3.10E-02 ......... ......... ......... .........  9.59E-03",
				"",
				" OM12",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM13",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM22",
				"+       -1.25E-01  1.41E-01  3.34E-02  2.03E-02  3.38E-02 ......... ......... ......... .........  1.05E-01 ......... .........",
				"         3.53E-03",
				"",
				" OM23",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"         ......... .........",
				"",
				" OM33",
				"+        9.00E-02 -2.64E-02  4.10E-02 -2.81E-02  6.29E-02 ......... ......... ......... .........  8.87E-02 ......... .........",
				"         -4.90E-02 .........  1.86E-03",
				"",
				" SG11",
				"+       -3.78E-02  8.42E-02 -1.56E-01 -1.15E-01  9.65E-02 ......... ......... ......... ......... -1.93E-01 ......... .........",
				"         9.53E-01 ......... -9.96E-01  7.34E-05",
				"",
			},
			n:              3,
			correlationsOk: false,
			context:        "Not OK, last row",
		},
	}

	for _, test := range tests {
		tt.Run(test.context, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			correlationsOk := getCorrelationStatus(test.lines, test.n, 0.90)
			t.A.Equal(test.correlationsOk, correlationsOk, "Fail :"+test.context)
		})
	}
}

// These tests show there is little improvement to be gained by optimizing the
// code for a lower diagonal matrix, or using for loops instead of range.
func BenchmarkCheckMatrix1(b *testing.B) {
	dim := 10000
	matrix := make([][]float64, dim)
	for i := range matrix {
		matrix[i] = make([]float64, dim)
	}
	for j := range matrix {
		for k := range matrix[j] {
			matrix[j][k] = rand.Float64()
		}
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		checkMatrix(matrix, 1)
	}
}

func BenchmarkCheckMatrix2(b *testing.B) {
	dim := 10000
	matrix := make([][]float64, dim)
	for i := range matrix {
		matrix[i] = make([]float64, dim)
	}
	for j := range matrix {
		for k := range matrix[j] {
			matrix[j][k] = rand.Float64()
		}
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		checkMatrix2(matrix, 1)
	}
}

func checkMatrix(matrix [][]float64, limit float64) bool {
	for j := range matrix {
		for k, cell := range matrix[j] {
			if k > (j + 1) {
				break
			}
			if k == j || cell == 0 {
				continue
			}
			if math.Abs(cell) >= limit {
				return false
			}
		}
	}

	return true
}

func checkMatrix2(matrix [][]float64, limit float64) bool {
	dim := len(matrix)
	for j := 0; j < dim; j++ {
		for k := 0; k < j; k++ {
			if k == j || matrix[j][k] == 0 {
				continue
			}
			if math.Abs(matrix[j][k]) >= limit {
				return false
			}
		}
	}

	return true
}

func TestCheckMatrix(tt *testing.T) {
	t := wrapt.WrapT(tt)
	dim := 3
	matrix := make([][]float64, dim)
	for i := range matrix {
		matrix[i] = make([]float64, dim)
	}

	count := 1.0
	for j := range matrix {
		for k := range matrix[j] {
			matrix[j][k] = count
			count++
		}
	}

	count = 1.0
	for j := range matrix {
		for k := range matrix[j] {
			t.A.Equal(matrix[j][k], count, "Fail")
			count++
		}
	}

	// change to column major
	matrix = transpose(matrix)
	count = 1.0
	for k := range matrix {
		for j := range matrix[k] {
			t.A.Equal(matrix[j][k], count, "Fail")
			count++
		}
	}
}

func TestSetCov(tt *testing.T) {
	var tests = []struct {
		lines   []string
		n       int
		context string
	}{

		{
			lines: []string{
				"************************************************************************************************************************",
				"********************                                                                                ********************",
				"********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************",
				"********************                          CORRELATION MATRIX OF ESTIMATE                        ********************",
				"********************                                                                                ********************",
				"************************************************************************************************************************",
				"                                                                                                                        ",
				"                                                                                                                        ",
				"		   TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      TH 9      OM11      OM12      OM13",
				"			OM22      OM23      OM33      SG11",
				"",
				" TH 1",
				"+        6.09E-01",
				"",
				" TH 2",
				"+       -7.99E-02  4.41E+00",
				"",
				" TH 3",
				"+       -1.06E-01 -3.44E-01  2.33E+00",
				"",
				" TH 4",
				"+       -3.91E-02 -4.46E-01  6.10E-01  1.01E+00",
				"",
				" TH 5",
				"+        8.05E-02  3.89E-01 -5.73E-01 -7.30E-01  2.13E-02",
				"",
				" TH 6",
				"+       6.0 ......... ......... ......... ......... .........",
				"",
				" TH 7",
				"+       7.0 ......... ......... ......... ......... ......... .........",
				"",
				" TH 8",
				"+       8.0 ......... ......... ......... ......... ......... ......... .........",
				"",
				" TH 9",
				"+       9.0 ......... ......... ......... ......... ......... ......... ......... 9.9",
				"",
				" OM11",
				"+        6.62E-02 -5.65E-02 -2.33E-02  7.85E-02  3.10E-02 ......... ......... ......... .........  9.59E-03",
				"",
				" OM12",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM13",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"",
				" OM22",
				"+       -1.25E-01  1.41E-01  3.34E-02  2.03E-02  3.38E-02 ......... ......... ......... .........  1.05E-01 ......... .........",
				"         3.53E-03",
				"",
				" OM23",
				"+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........",
				"         ......... .........",
				"",
				" OM33",
				"+        9.00E-02 -2.64E-02  4.10E-02 -2.81E-02  6.29E-02 ......... ......... ......... .........  8.87E-02 ......... .........",
				"         -4.90E-02 .........  1.86E-03",
				"",
				" SG11",
				"+       -3.78E-02  8.42E-02 -1.56E-01 -1.15E-01  9.65E-02 ......... ......... ......... ......... -1.93E-01 ......... .........",
				"         9.53E-03 ......... -7.70E-02  7.34E-05",
				"",
				"",
				"1",
			},
			n:       3,
			context: "OK",
		},
	}

	for _, test := range tests {
		tt.Run(test.context, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			covTheta := getThetaValues(test.lines, test.n)
			t.A.Equal(9, covTheta.Dim, "Fail :"+test.context)
			t.A.Equal(0.609, covTheta.Values[0], "Fail :"+test.context)
			t.A.Equal(-0.0799, covTheta.Values[1], "Fail :"+test.context)
			t.A.Equal(8.0, covTheta.Values[7], "Fail :"+test.context)
			t.A.Equal(9.9, covTheta.Values[80], "Fail :"+test.context)
		})
	}
}

func TestGetGradientLine(tt *testing.T) {
	var tests = []struct {
		context  string
		lines    []string
		n        int
		expected string
	}{
		{
			context: "only test",
			lines: []string{
				"GRADIENT:  -2.9680E-01  6.9220E-01  3.9483E+00 -4.6290E+00  8.4163E-02  6.2807E-02  1.8689E-01  6.9662E-02  1.8307E-01  8.3694E-01",
				"             0.0000E+00",
				"",
			},
			n:        0,
			expected: "GRADIENT:  -2.9680E-01  6.9220E-01  3.9483E+00 -4.6290E+00  8.4163E-02  6.2807E-02  1.8689E-01  6.9662E-02  1.8307E-01  8.3694E-01             0.0000E+00",
		},
	}

	for _, test := range tests {
		tt.Run(test.context, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			line := getGradientLine(test.lines, test.n)
			t.A.Equal(test.expected, line, "Fail :"+test.expected)
			hasZero := parseGradient([]string{line})
			t.A.Equal(true, hasZero, "Fail :"+test.expected)
		})
	}
}
