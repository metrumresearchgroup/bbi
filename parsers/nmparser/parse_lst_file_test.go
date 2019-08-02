package parser

import (
	"testing"
)

func TestParseOBJV(t *testing.T) {
	ofvDetails := new(OfvDetails)

	parseOFV("OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT: 821.705", ofvDetails)
	if ofvDetails.OFVNoConstant != 821.705 {
		t.Log("\nGOT: ", ofvDetails.OFVNoConstant, "\n Expected: 821.705")
		t.Fail()
	}

	parseOFV("OBJECTIVE FUNCTION VALUE WITH CONSTANT:       1639.561", ofvDetails)
	if ofvDetails.OFVWithConstant != 1639.561 || ofvDetails.OFVNoConstant != 821.705 {
		t.Log("\nGOT: ", ofvDetails.OFVWithConstant, "\n Expected: 1639.561")
		t.Log("\nGOT: ", ofvDetails.OFVNoConstant, "\n Expected: 821.705")
		t.Fail()
	}

	parseOFV("N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    817.855", ofvDetails)
	if ofvDetails.OFV != 817.855 || ofvDetails.OFVWithConstant != 1639.561 || ofvDetails.OFVNoConstant != 821.705 {
		t.Log("\nGOT: ", ofvDetails.OFV, "\n Expected: 817.855")
		t.Log("\nGOT: ", ofvDetails.OFVWithConstant, "\n Expected: 1639.561")
		t.Log("\nGOT: ", ofvDetails.OFVNoConstant, "\n Expected: 821.705")
		t.Fail()
	}

	parseOFV("#OBJV:********************************************    -7913.528       **************************************************", ofvDetails)
	if ofvDetails.OFV != 817.855 || ofvDetails.OFVWithConstant != 1639.561 || ofvDetails.OFVNoConstant != -7913.528 {
		t.Log("\nGOT: ", ofvDetails.OFV, "\n Expected: 817.855")
		t.Log("\nGOT: ", ofvDetails.OFVWithConstant, "\n Expected: 1639.561")
		t.Log("\nGOT: ", ofvDetails.OFVNoConstant, "\n Expected: 821.705")
		t.Fail()
	}
}

func TestParTestParseOBJV2(t *testing.T) {
	var lines = []string{
		"OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT: 821.705",
		"OBJECTIVE FUNCTION VALUE WITH CONSTANT:       1639.561",
		"N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    817.855",
		"#OBJV:********************************************    -7913.528       **************************************************",
	}
	lstData := ParseLstEstimationFile(lines)
	if lstData.OFV.OFV != 817.855 || lstData.OFV.OFVWithConstant != 1639.561 || lstData.OFV.OFVNoConstant != -7913.528 {
		t.Log("\nGOT: ", lstData.OFV.OFV, "\n Expected: 817.855")
		t.Log("\nGOT: ", lstData.OFV.OFVWithConstant, "\n Expected: 1639.561")
		t.Log("\nGOT: ", lstData.OFV.OFVNoConstant, "\n Expected: -7913.528")
		t.Fail()
	}
}

func TestParTestParseOBJV3(t *testing.T) {
	var lines = []string{
		"#OBJV:********************************************    -7913.528       **************************************************",
		"OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT: 821.705",
		"OBJECTIVE FUNCTION VALUE WITH CONSTANT:       1639.561",
		"N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    817.855",
	}
	lstData := ParseLstEstimationFile(lines)
	if lstData.OFV.OFV != 817.855 || lstData.OFV.OFVWithConstant != 1639.561 || lstData.OFV.OFVNoConstant != 821.705 {
		t.Log("\nGOT: ", lstData.OFV.OFV, "\n Expected: 817.855")
		t.Log("\nGOT: ", lstData.OFV.OFVWithConstant, "\n Expected: 1639.561")
		t.Log("\nGOT: ", lstData.OFV.OFVNoConstant, "\n Expected: 821.705")
		t.Fail()
	}
}
