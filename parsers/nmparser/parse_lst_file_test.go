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
