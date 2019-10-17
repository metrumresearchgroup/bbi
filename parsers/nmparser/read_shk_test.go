package parser

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestReadParseShkLines(t *testing.T) {
	var tests = []struct {
		lines     []string
		expected1 string
		expected2 string
		context   string
	}{
		{
			lines: []string{
				"TABLE NO.     1: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				"TYPE          SUBPOP       ETA(1)       ETA(2)       ETA(3)       ETA(4)",
				"            1            1 -3.67026E-03 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            2            1  3.19719E-02  2.60947E-02  1.62789E-02  1.89996E-02",
				"            3            1  9.08606E-01  8.80753E-01  6.22835E-01  9.17950E-01",
				"            4            1  3.82499E+00  2.52933E+01  5.46357E+01  3.69109E+01",
				"            5            1  1.32691E+01  0.00000E+00  0.00000E+00  0.00000E+00",
				"            6            1  3.79795E+00  2.56533E+01  5.45618E+01  3.71115E+01",
				"            7            1  7.90000E+01  7.90000E+01  7.90000E+01  7.90000E+01",
				"            8            1  7.50368E+00  4.41891E+01  7.94208E+01  6.01976E+01",
				"            9            1  7.45166E+00  4.47258E+01  7.93537E+01  6.04504E+01",
				"            10            1  2.47776E+01  0.00000E+00  0.00000E+00  0.00000E+00",
				"TABLE NO.     2: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				"TYPE          SUBPOP       ETA(1)       ETA(2)       ETA(3)       ETA(4)",
				"            1            1 -3.67026E-03 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            2            1  3.19719E-02  2.60947E-02  1.62789E-02  1.89996E-02",
				"            3            1  9.08606E-01  8.80753E-01  6.22835E-01  9.17950E-01",
				"            4            1  3.82499E+00  2.52933E+01  5.46357E+01  3.69109E+01",
				"            5            1  1.32691E+01  0.00000E+00  0.00000E+00  0.00000E+00",
				"            6            1  3.79795E+00  2.56533E+01  5.45618E+01  3.71115E+01",
				"            7            1  7.90000E+01  7.90000E+01  7.90000E+01  7.90000E+01",
				"            8            1  7.50368E+00  4.41891E+01  7.94208E+01  6.01976E+01",
				"            9            1  7.45166E+00  4.47258E+01  7.93537E+01  6.04504E+01",
				"            10            1  2.47776E+01  0.00000E+00  0.00000E+00  0.00000E+00",
			},
			expected1: "TABLE NO.     1: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
			expected2: "TABLE NO.     2: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
			context:   "2 methods",
		},
	}

	for _, tt := range tests {

		ed := ParseShkLines(tt.lines)
		assert.Equal(t, tt.expected1, ed.EstimationMethods[0], "Fail :"+tt.context)
		assert.Equal(t, tt.expected2, ed.EstimationMethods[1], "Fail :"+tt.context)
		assert.Equal(t, "TYPE", ed.ParameterNames[0], "Fail :"+tt.context)
		assert.Equal(t, "ETA(4)", ed.ParameterNames[5], "Fail :"+tt.context)
		assert.Equal(t, "1            1 -3.67026E-03 -3.91460E-03 -8.00654E-03 -1.95727E-03", ed.EstimationLines[0][0], "Fail :"+tt.context)

		shk := ParseShrinkage(ed.EstimationLines[0])
		assert.Equal(t, -0.00367026, shk.EtaBar[0], "Fail :"+tt.context)

		sd := ParseShkData(ed)
		assert.Equal(t, -0.00367026, sd[0].EtaBar[0], "Fail :"+tt.context)
		assert.Equal(t, 24.7776, sd[1].EbvVR[0], "Fail :"+tt.context)
		assert.Equal(t, float64(0), sd[1].EbvVR[3], "Fail :"+tt.context)
	}
}
