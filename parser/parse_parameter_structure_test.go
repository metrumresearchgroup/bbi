package parser

import (
	"testing"
)

func TestParseParameterStructure(t *testing.T) {
	var parameterDiagonal01 = []string{"0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2"}
	var parameterBlock01 = []string{
		"0OMEGA HAS BLOCK FORM:",
		" 1",
		" 0  2",
		" 0  2  2",
		" 0  0  0  3",
	}
	var parameterDiagonal01Results = []int{1, 0, 1}
	var parameterBlock01Results = []int{1, 0, 2, 0, 2, 2, 0, 0, 0, 3}

	parsedDiagonal01 := parseParameterStructure(parameterDiagonal01)
	for i, val := range parsedDiagonal01 {
		if val != parameterDiagonal01Results[i] {
			t.Log("GOT: ", val, " EXPECTED: ", parameterBlock01Results[i], " at index: ", i)
			t.Fail()
		}
	}

	parsedBlock01 := parseParameterStructure(parameterBlock01)
	for i, val := range parsedBlock01 {
		if val != parameterBlock01Results[i] {
			t.Log("GOT: ", val, " EXPECTED: ", parameterBlock01Results[i], " at index: ", i)
			t.Fail()
		}
	}
}
