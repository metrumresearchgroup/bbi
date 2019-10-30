package parser

import (
	"math"
	"math/rand"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseOBJV(t *testing.T) {
	var ofvDetails OfvDetails
	var expected OfvDetails

	expected.OFVNoConstant = 821.705
	ofvDetails = parseOFV("OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT: 821.705", ofvDetails)
	assert.Equal(t, expected, ofvDetails)

	expected.OFVWithConstant = 1639.561
	ofvDetails = parseOFV("OBJECTIVE FUNCTION VALUE WITH CONSTANT:       1639.561", ofvDetails)
	assert.Equal(t, expected, ofvDetails)

	expected.OFV = 817.855
	ofvDetails = parseOFV("N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    817.855", ofvDetails)
	assert.Equal(t, expected, ofvDetails)

	expected.OFVNoConstant = -7913.528
	ofvDetails = parseOFV("#OBJV:********************************************    -7913.528       **************************************************", ofvDetails)
	assert.Equal(t, expected, ofvDetails)
}

func TestParTestParseOBJV2(t *testing.T) {
	var lines = []string{
		"OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT: 821.705",
		"OBJECTIVE FUNCTION VALUE WITH CONSTANT:       1639.561",
		"N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    817.855",
		"#OBJV:********************************************    -7913.528       **************************************************",
	}
	expected := OfvDetails{
		OFV:             817.855,
		OFVWithConstant: 1639.561,
		OFVNoConstant:   -7913.528,
	}
	lstData := ParseLstEstimationFile(lines)
	assert.Equal(t, lstData.OFV, expected)
}

func TestParTestParseOBJV3(t *testing.T) {
	var lines = []string{
		"#OBJV:********************************************    -7913.528       **************************************************",
		"OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT: 821.705",
		"OBJECTIVE FUNCTION VALUE WITH CONSTANT:       1639.561",
		"N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    817.855",
	}
	expected := OfvDetails{
		OFV:             817.855,
		OFVWithConstant: 1639.561,
		OFVNoConstant:   821.705,
	}
	lstData := ParseLstEstimationFile(lines)
	assert.Equal(t, expected, lstData.OFV)
}

func TestParTestParseShrinkage(t *testing.T) {
	var shrinkageDetails ShrinkageDetails
	var expected ShrinkageDetails

	expected.EtaSD = []float64{4.0774, 29.015, 11.401}
	shrinkageDetails = parseShrinkage("ETASHRINKSD(%)  4.0774E+00  2.9015E+01  1.1401E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.EtaVR = []float64{7.9885, 49.611, 21.502}
	shrinkageDetails = parseShrinkage("ETASHRINKVR(%)  7.9885E+00  4.9611E+01  2.1502E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.EbvSD = []float64{4.0725, 28.322, 12.255}
	shrinkageDetails = parseShrinkage("EBVSHRINKSD(%)  4.0725E+00  2.8322E+01  1.2255E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.EbvVR = []float64{7.9791, 48.623, 23.009}
	shrinkageDetails = parseShrinkage("EBVSHRINKVR(%)  7.9791E+00  4.8623E+01  2.3009E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.EpsSD = []float64{12.507, 12.507}
	shrinkageDetails = parseShrinkage("EPSSHRINKSD(%)  1.2507E+01  1.2507E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.EpsVR = []float64{23.451, 23.451}
	shrinkageDetails = parseShrinkage("EPSSHRINKVR(%)  2.3451E+01  2.3451E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)
}

func TestParTestParseShrinkage2(t *testing.T) {
	var lines = []string{
		"ETASHRINKSD(%)  4.0774E+00  2.9015E+01  1.1401E+01",
		"ETASHRINKVR(%)  7.9885E+00  4.9611E+01  2.1502E+01",
		"EBVSHRINKSD(%)  4.0725E+00  2.8322E+01  1.2255E+01",
		"EBVSHRINKVR(%)  7.9791E+00  4.8623E+01  2.3009E+01",
		"EPSSHRINKSD(%)  1.2507E+01  1.2507E+01",
		"EPSSHRINKVR(%)  2.3451E+01  2.3451E+01",
	}
	expected := ShrinkageDetails{
		EtaSD: []float64{4.0774, 29.015, 11.401},
		EtaVR: []float64{7.9885, 49.611, 21.502},

		EbvSD: []float64{4.0725, 28.322, 12.255},
		EbvVR: []float64{7.9791, 48.623, 23.009},

		EpsSD: []float64{12.507, 12.507},
		EpsVR: []float64{23.451, 23.451},
	}
	lstData := ParseLstEstimationFile(lines)
	assert.Equal(t, expected, lstData.ShrinkageDetails[0])
}

func TestParseGradient(t *testing.T) {

	var tests = []struct {
		lines                []string
		hasZeroFinalGradient HeuristicStatus
		context              string
	}{
		{
			lines: []string{
				"GRADIENT:  -3.8561E+01  4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   8.7727E+01  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   4.8447E+01  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   1.0154E-02  1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroFinalGradient: HeuristicFalse,
			context:              "no zero gradient",
		},
		{
			lines: []string{
				"GRADIENT:   0           4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   8.7727E+01  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   4.8447E+01  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   1.0154E-02  1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroFinalGradient: HeuristicFalse,
			context:              "zero gradient",
		},
		{
			lines: []string{
				"GRADIENT:  -3.8561E+01  4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   8.7727E+01  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   4.8447E+01  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   0           1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroFinalGradient: HeuristicTrue,
			context:              "zero final gradient",
		},
		{
			lines: []string{
				"GRADIENT:   0  4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   0  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   0  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   0  1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroFinalGradient: HeuristicTrue,
			context:              "zero gradients and zero final gradient",
		},
		// {
		// 	hasZeroGradient:      nil,
		// 	hasZeroFinalGradient: nil,
		// 	context:              "zero gradients and zero final gradient",
		// },
	}

	for _, tt := range tests {
		noZeroFinal := parseGradient(tt.lines)
		assert.Equal(t, tt.hasZeroFinalGradient, noZeroFinal, "Fail hasZeroFinalGradient: "+tt.context)
	}
}

func TestSetLargeConditionNumber(t *testing.T) {
	var tests = []struct {
		lines                []string
		n                    int
		largeConditionNumber HeuristicStatus
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
			n:                    3,
			largeConditionNumber: HeuristicFalse,
			context:              "not large",
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
			n:                    3,
			largeConditionNumber: HeuristicTrue,
			context:              "large",
		},
		// {
		// 	lines:                []string{"", ""},
		// 	n:                    0,
		// 	largeConditionNumber: Undefined,
		// 	context:              "not set",
		// },
	}

	for _, tt := range tests {

		largeConditionNumber := getLargeConditionNumberStatus(tt.lines, tt.n, 1000.0)
		assert.Equal(t, tt.largeConditionNumber, largeConditionNumber, "Fail :"+tt.context)
	}
}

func TestSetCorrelationsOk(t *testing.T) {
	var tests = []struct {
		lines          []string
		n              int
		correlationsOk HeuristicStatus
		context        string
	}{
		{
			lines:          []string{"", ""},
			n:              0,
			correlationsOk: HeuristicUndefined,
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
			correlationsOk: HeuristicTrue,
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
			correlationsOk: HeuristicFalse,
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
			correlationsOk: HeuristicFalse,
			context:        "Not OK, last row",
		},
	}

	for _, tt := range tests {
		correlationsOk := getCorrelationStatus(tt.lines, tt.n, 0.90)
		assert.Equal(t, tt.correlationsOk, correlationsOk, "Fail :"+tt.context)
		//assert.Equal(t, true, false, "Fail :"+tt.context)
	}
}

// These tests show there is little improvement to be gained by optimizing the
// code for a lower diagonal matrix, or using for loops instead of range
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

func TestCheckMatrix(t *testing.T) {
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
			assert.Equal(t, matrix[j][k], count, "Fail")
			count++
		}
	}

	// change to column major
	matrix = transpose(matrix)
	count = 1.0
	for k := range matrix {
		for j := range matrix[k] {
			assert.Equal(t, matrix[j][k], count, "Fail")
			count++
		}
	}
}

func TestSetCov(t *testing.T) {
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

	for _, tt := range tests {
		covTheta := getThetaValues(tt.lines, tt.n)
		assert.Equal(t, 9, covTheta.Dim, "Fail :"+tt.context)
		assert.Equal(t, 0.609, covTheta.Values[0], "Fail :"+tt.context)
		assert.Equal(t, -0.0799, covTheta.Values[1], "Fail :"+tt.context)
		assert.Equal(t, 8.0, covTheta.Values[7], "Fail :"+tt.context)
		assert.Equal(t, 9.9, covTheta.Values[80], "Fail :"+tt.context)
	}
}

func TestGetGradientLine(t *testing.T) {
	var tests = []struct {
		lines    []string
		n        int
		expected string
	}{
		{
			lines: []string{
				"GRADIENT:  -2.9680E-01  6.9220E-01  3.9483E+00 -4.6290E+00  8.4163E-02  6.2807E-02  1.8689E-01  6.9662E-02  1.8307E-01  8.3694E-01",
				"             0.0000E+00",
				"",
			},
			n:        0,
			expected: "GRADIENT:  -2.9680E-01  6.9220E-01  3.9483E+00 -4.6290E+00  8.4163E-02  6.2807E-02  1.8689E-01  6.9662E-02  1.8307E-01  8.3694E-01             0.0000E+00",
		},
	}

	for _, tt := range tests {
		line := getGradientLine(tt.lines, tt.n)
		assert.Equal(t, tt.expected, line, "Fail :"+tt.expected)
		hasZero := parseGradient([]string{line})
		assert.Equal(t, HeuristicTrue, hasZero, "Fail :"+tt.expected)
	}
}
