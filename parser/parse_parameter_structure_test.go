package parser

import (
	"reflect"
	"testing"
)

// unit tests

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

// integration test of sorts

func TestParseParameterStructures(t *testing.T) {
	var noBlocks01 = []string{
		"0LENGTH OF THETA:   5",
		"0DEFAULT THETA BOUNDARY TEST OMITTED:    NO",
		"0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2",
		"0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO",
		"0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1",
		"0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO",
	}
	noBlocks01Results := ParameterStructures{
		Theta: 5,
		Omega: []int{1, 0, 1},
		Sigma: []int{1},
	}
	parsedNoBlock01 := ParseParameterStructures(noBlocks01)
	if !reflect.DeepEqual(parsedNoBlock01.Theta, noBlocks01Results.Theta) {
		t.Log("GOT: ", parsedNoBlock01.Theta, " EXPECTED: ", noBlocks01Results.Theta)
		t.Fail()
	}
	if !reflect.DeepEqual(parsedNoBlock01.Omega, noBlocks01Results.Omega) {
		t.Log("GOT: ", parsedNoBlock01.Omega, " EXPECTED: ", noBlocks01Results.Omega)
		t.Fail()
	}
	if !reflect.DeepEqual(parsedNoBlock01.Sigma, noBlocks01Results.Sigma) {
		t.Log("GOT: ", parsedNoBlock01.Sigma, " EXPECTED: ", noBlocks01Results.Sigma)
		t.Fail()
	}

	var blocks01 = []string{
		"0LENGTH OF THETA:   4",
		"0DEFAULT THETA BOUNDARY TEST OMITTED:    NO",
		"0OMEGA HAS BLOCK FORM:",
		"  1",
		"  0  2",
		"  0  2  2",
		"  0  0  0  3",
		"0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO",
		"0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1",
		"0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO",
	}
	blocks01Results := ParameterStructures{
		Theta: 4,
		Omega: []int{1, 0, 2, 0, 2, 2, 0, 0, 0, 3},
		Sigma: []int{1},
	}
	parsedBlock01 := ParseParameterStructures(blocks01)
	if !reflect.DeepEqual(parsedBlock01.Theta, blocks01Results.Theta) {
		t.Log("GOT: ", parsedBlock01.Theta, " EXPECTED: ", blocks01Results.Theta)
		t.Fail()
	}
	if !reflect.DeepEqual(parsedBlock01.Omega, blocks01Results.Omega) {
		t.Log("GOT: ", parsedBlock01.Omega, " EXPECTED: ", blocks01Results.Omega)
		t.Fail()
	}
	if !reflect.DeepEqual(parsedBlock01.Sigma, blocks01Results.Sigma) {
		t.Log("GOT: ", parsedBlock01.Sigma, " EXPECTED: ", blocks01Results.Sigma)
		t.Fail()
	}
}
