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
