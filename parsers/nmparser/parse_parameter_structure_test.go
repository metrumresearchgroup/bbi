package parser

import (
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

// unit tests

func TestParseParameterStructure(tt *testing.T) {
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

	testId := "UNIT-NMP-015"
	tt.Run(testId, func(tt *testing.T) {

		t := wrapt.WrapT(tt)

		parsedDiagonal01 := parseParameterStructure(parameterDiagonal01)

		t.R.Equal(parameterDiagonal01Results, parsedDiagonal01)

		parsedBlock01 := parseParameterStructure(parameterBlock01)

		t.R.Equal(parameterBlock01Results, parsedBlock01)
	})
}

// integration test of sorts

func TestParseParameterStructures(tt *testing.T) {
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
	testId := "UNIT-NMP-016"
	tt.Run(testId, func(tt *testing.T) {

		t := wrapt.WrapT(tt)

		parsedNoBlock01 := ParseParameterStructures(noBlocks01)

		t.R.Equal(noBlocks01Results, parsedNoBlock01)

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

		t.R.Equal(blocks01Results, parsedBlock01)
	})
}
