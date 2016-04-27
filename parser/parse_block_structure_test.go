package parser

import (
	"testing"
)

var parameterBlock01 = []string{
	" 1",
	" 0  2",
	" 0  2  2",
	" 0  0  0  3",
}
var parameterBlock01Results = []int{1, 0, 2, 0, 2, 2, 0, 0, 0, 3}

func TestParseParameterBlock(t *testing.T) {
	parsedData := ParseBlockStructure(parameterBlock01)
	for i, val := range parsedData {
		if val != parameterBlock01Results[i] {
			t.Log("GOT: ", val, " EXPECTED: ", parameterBlock01Results[i], " at index: ", i)
			t.Fail()
		}
	}
}
