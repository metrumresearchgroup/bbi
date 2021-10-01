package parser

import (
	"github.com/metrumresearchgroup/bbi/utils"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestDefaultParameterNames(tt *testing.T) {
	tests := []struct {
		values   map[string]int
		expected ParameterNames
		context  string
	}{
		{
			values: map[string]int{
				"nTheta": 1,
				"nOmega": 1,
				"nSigma": 1,
			},
			expected: ParameterNames{
				Theta: []string{"THETA1"},
				Omega: []string{"OMEGA(1,1)"},
				Sigma: []string{"SIGMA(1,1)"},
			},
			context: "1 param for all",
		},
		{
			values: map[string]int{
				"nTheta": 2,
				"nOmega": 3,
				"nSigma": 6,
			},
			expected: ParameterNames{
				Theta: []string{"THETA1", "THETA2"},
				Omega: []string{"OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)"},
				Sigma: []string{"SIGMA(1,1)", "SIGMA(2,1)", "SIGMA(2,2)",
					"SIGMA(3,1)", "SIGMA(3,2)", "SIGMA(3,3)"},
			},
			context: "multiple params",
		},
	}
	testId := "UNIT-NMP-011"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.context, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			pn := NewDefaultParameterNames(test.values["nTheta"], test.values["nOmega"], test.values["nSigma"])

			t.R.Equal(test.expected, pn)
		})
	}
}
