package parser

import (
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

	expected.Eta.SD = []float64{4.0774, 29.015, 11.401}
	shrinkageDetails = parseShrinkage("ETASHRINKSD(%)  4.0774E+00  2.9015E+01  1.1401E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.Eta.VR = []float64{7.9885, 49.611, 21.502}
	shrinkageDetails = parseShrinkage("ETASHRINKVR(%)  7.9885E+00  4.9611E+01  2.1502E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.Ebv.SD = []float64{4.0725, 28.322, 12.255}
	shrinkageDetails = parseShrinkage("EBVSHRINKSD(%)  4.0725E+00  2.8322E+01  1.2255E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.Ebv.VR = []float64{7.9791, 48.623, 23.009}
	shrinkageDetails = parseShrinkage("EBVSHRINKVR(%)  7.9791E+00  4.8623E+01  2.3009E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.Eps.SD = []float64{12.507, 12.507}
	shrinkageDetails = parseShrinkage("EPSSHRINKSD(%)  1.2507E+01  1.2507E+01", shrinkageDetails)
	assert.Equal(t, expected, shrinkageDetails)

	expected.Eps.VR = []float64{23.451, 23.451}
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
		Eta: Shrinkage{
			SD: []float64{4.0774, 29.015, 11.401},
			VR: []float64{7.9885, 49.611, 21.502},
		},
		Ebv: Shrinkage{
			SD: []float64{4.0725, 28.322, 12.255},
			VR: []float64{7.9791, 48.623, 23.009},
		},
		Eps: Shrinkage{
			SD: []float64{12.507, 12.507},
			VR: []float64{23.451, 23.451},
		},
	}
	lstData := ParseLstEstimationFile(lines)
	assert.Equal(t, expected, lstData.ShrinkageDetails)
}

func TestParseGradient(t *testing.T) {

	var tests = []struct {
		lines                []string
		hasZeroGradient      *bool
		hasZeroFinalGradient *bool
		context              string
	}{
		{
			lines: []string{
				"GRADIENT:  -3.8561E+01  4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   8.7727E+01  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   4.8447E+01  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   1.0154E-02  1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroGradient:      newBool(false),
			hasZeroFinalGradient: newBool(false),
			context:              "no zero gradient",
		},
		{
			lines: []string{
				"GRADIENT:   0           4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   8.7727E+01  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   4.8447E+01  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   1.0154E-02  1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroGradient:      newBool(true),
			hasZeroFinalGradient: newBool(false),
			context:              "zero gradient",
		},
		{
			lines: []string{
				"GRADIENT:  -3.8561E+01  4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   8.7727E+01  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   4.8447E+01  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   0           1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroGradient:      newBool(true),
			hasZeroFinalGradient: newBool(true),
			context:              "zero final gradient",
		},
		{
			lines: []string{
				"GRADIENT:   0  4.3326E+01  2.3360E+02  2.1230E+02  2.1296E+02 -5.0598E+00  2.8695E+01 -1.5814E+01  6.8174E+01",
				"GRADIENT:   0  1.2173E+02 -7.6623E+01 -3.8494E+01  1.1100E+01  1.4942E+00 -7.9555E-01 -7.8345E+00 -1.7576E+01",
				"GRADIENT:   0  5.7713E+01 -3.7520E+01 -9.4538E+00 -3.7976E+00  6.1177E+00 -3.1052E+00  3.6826E+00 -2.0215E+01",
				"GRADIENT:   0  1.2524E-03  1.9361E-02 -6.2542E-02  4.4554E-02 -1.4700E-02 -2.3183E-02 -5.9024E-03 -5.8699E-03",
			},
			hasZeroGradient:      newBool(true),
			hasZeroFinalGradient: newBool(true),
			context:              "zero gradients and zero final gradient",
		},
		{
			hasZeroGradient:      nil,
			hasZeroFinalGradient: nil,
			context:              "zero gradients and zero final gradient",
		},
	}

	for _, tt := range tests {
		noZero, noZeroFinal := parseGradient(tt.lines)
		assert.Equal(t, tt.hasZeroGradient, noZero, "Fail hasZeroGradient:"+tt.context)
		assert.Equal(t, tt.hasZeroFinalGradient, noZeroFinal, "Fail hasZeroFinalGradient: "+tt.context)
	}
}

func newBool(value bool) *bool {
	b := value
	return &b
}
