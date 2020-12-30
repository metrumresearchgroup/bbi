package parser

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseEstimatesFromExt(t *testing.T) {
	var tests = []struct {
		path   string
		expected ExtFastData
		context string
	}{
	  {path: "testdata/extdata/single-table-01.ext",
	  	expected: ExtFastData{
	  	ParameterNames: []string{"THETA1", "THETA2", "THETA3", "THETA4", "THETA5", "THETA6", "THETA7", "THETA8", "THETA9", "THETA10", "THETA11", "THETA12", "THETA13", "THETA14", "SIGMA(1,1)", "SIGMA(2,1)", "SIGMA(2,2)", "SIGMA(3,1)", "SIGMA(3,2)", "SIGMA(3,3)", "SIGMA(4,1)", "SIGMA(4,2)", "SIGMA(4,3)", "SIGMA(4,4)", "OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)", "OMEGA(3,1)", "OMEGA(3,2)", "OMEGA(3,3)", "OMEGA(4,1)", "OMEGA(4,2)", "OMEGA(4,3)", "OMEGA(4,4)", "OMEGA(5,1)", "OMEGA(5,2)", "OMEGA(5,3)", "OMEGA(5,4)", "OMEGA(5,5)"},
		EstimationLines: [][]string{
	  		{
				"2.50795E+00",
				"-2.15184E+00",
				"-7.01976E-01",
				"3.12308E+00",
				"-5.41540E-01",
				"1.00000E+00",
				"-4.56642E-01",
				"3.57043E-01",
				"-1.16279E-01",
				"-1.34025E-02",
				"1.36235E-01",
				"-6.50787E-01",
				"0.00000E+00",
				"-1.02530E-01",
				"5.19801E+00",
				"0.00000E+00",
				"9.75263E-02",
				"0.00000E+00",
				"0.00000E+00",
				"1.98807E+00",
				"0.00000E+00",
				"0.00000E+00",
				"0.00000E+00",
				"1.74399E-01",
				"9.66816E-02",
				"0.00000E+00",
				"6.92143E-01",
				"0.00000E+00",
				"0.00000E+00",
				"4.71456E-02",
				"0.00000E+00",
				"0.00000E+00",
				"0.00000E+00",
				"4.71456E-02",
				"0.00000E+00",
				"0.00000E+00",
				"0.00000E+00",
				"0.00000E+00",
				"4.71456E-02",
			},
		},
		},
			context: "single table",
		},
	}

	for _, tt := range tests {

		res, err := ParseEstimatesFromExt(tt.path)
		if err != nil {
			assert.Fail(t, "error reading path", tt.path, err)
		}
		assert.Equal(t, tt.expected.ParameterNames, res.ParameterNames, "Fail :"+tt.context)
		assert.Equal(t, tt.expected.EstimationLines, res.EstimationLines, "Fail :"+tt.context)
	}
}
