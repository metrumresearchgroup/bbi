package parser

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDefaultParameterNames(t *testing.T) {
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
	for _, tt := range tests {
		t.Run(tt.context, func(t *testing.T) {
			pn := NewDefaultParameterNames(tt.values["nTheta"], tt.values["nOmega"], tt.values["nSigma"])
			assert.Equal(t, tt.expected, pn, "Fail :"+tt.context)
		})
	}
}
