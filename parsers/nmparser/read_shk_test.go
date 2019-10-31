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
		expected3 string
		context   string
		etaCount  int
		epsCount  int
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
				"            10           1  2.47776E+01  0.00000E+00  0.00000E+00  0.00000E+00",
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
				"TABLE NO.     3: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				"TYPE          SUBPOP       ETA(1)       ETA(2)       ETA(3)       ETA(4)",
				"            1            1  1.67026E-03  2.91460E-03  3.00654E-03  4.95727E-03",
				"            2            1  3.19719E-02  2.60947E-02  1.62789E-02  1.89996E-02",
				"            3            1  9.08606E-01  8.80753E-01  6.22835E-01  9.17950E-01",
				"            4            1  3.82499E+00  2.52933E+01  5.46357E+01  3.69109E+01",
				"            5            1  1.32691E+01  0.00000E+00  0.00000E+00  0.00000E+00",
				"            6            1  3.79795E+00  2.56533E+01  5.45618E+01  3.71115E+01",
				"            7            1  7.90000E+01  7.90000E+01  7.90000E+01  7.90000E+01",
				"            8            1  7.50368E+00  4.41891E+01  7.94208E+01  6.01976E+01",
				"            9            1  7.45166E+00  4.47258E+01  7.93537E+01  6.04504E+01",
				"            10           1  2.47776E+01  0.00000E+00  0.00000E+00  0.00000E+00",
			},
			expected1: "TABLE NO.     1: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
			expected2: "TABLE NO.     2: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
			expected3: "TABLE NO.     3: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
			context:   "3 methods",
			etaCount:  4,
			epsCount:  1,
		},
	}

	for _, tt := range tests {

		ed := ParseShkLines(tt.lines)
		assert.Equal(t, tt.expected1, ed.EstimationMethods[0], "Fail :"+tt.context)
		assert.Equal(t, tt.expected2, ed.EstimationMethods[1], "Fail :"+tt.context)
		assert.Equal(t, tt.expected3, ed.EstimationMethods[2], "Fail :"+tt.context)
		assert.Equal(t, "TYPE", ed.ParameterNames[0], "Fail :"+tt.context)
		assert.Equal(t, "ETA(4)", ed.ParameterNames[5], "Fail :"+tt.context)
		assert.Equal(t, "1            1 -3.67026E-03 -3.91460E-03 -8.00654E-03 -1.95727E-03", ed.EstimationLines[0][0], "Fail :"+tt.context)
		assert.Equal(t, "1            1  1.67026E-03  2.91460E-03  3.00654E-03  4.95727E-03", ed.EstimationLines[2][0], "Fail :"+tt.context)

		shk := ParseShrinkage(ed.EstimationLines[0], tt.etaCount, tt.epsCount)
		assert.Equal(t, -0.00367026, shk[0].EtaBar[0], "Fail :"+tt.context)
		assert.Equal(t, 4, len(shk[0].EtaBar), "Fail :"+tt.context)

		sd := ParseShkData(ed, tt.etaCount, tt.epsCount)
		assert.Equal(t, -0.00367026, sd[0][0].EtaBar[0], "Fail :"+tt.context)
		assert.Equal(t, 7.45166, sd[1][0].EbvVR[0], "Fail :"+tt.context)
		assert.Equal(t, 60.4504, sd[1][0].EbvVR[3], "Fail :"+tt.context)

		assert.Equal(t, tt.epsCount, len(sd[0][0].EpsSD), "Fail :"+tt.context)
		assert.Equal(t, tt.epsCount, len(sd[2][0].EpsSD), "Fail :"+tt.context)
		assert.Equal(t, tt.epsCount, len(sd[0][0].EpsVR), "Fail :"+tt.context)
		assert.Equal(t, tt.epsCount, len(sd[2][0].EpsVR), "Fail :"+tt.context)

		assert.Equal(t, tt.etaCount, len(sd[0][0].EtaBar), "Fail :"+tt.context)
		assert.Equal(t, tt.etaCount, len(sd[2][0].EtaSD), "Fail :"+tt.context)
	}
}

func TestReadParseShkLines2(t *testing.T) {
	var tests = []struct {
		lines     []string
		expected1 string
		expected2 string
		expected3 string
		context   string
		etaCount  int
		epsCount  int
	}{
		{
			lines: []string{
				"TABLE NO.     1: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				"TYPE          SUBPOP       ETA(1)       ETA(2)       ETA(3)       ETA(4)",
				"            1            1  11.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            1            2  12.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            1            3  13.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",

				"            2            1  21.9719E-02  2.60947E-02  1.62789E-02  1.89996E-02",
				"            2            2  22.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            2            3  33.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",

				"            3            1  31.8606E-01  8.80753E-01  6.22835E-01  9.17950E-01",
				"            3            2  32.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            3            3  33.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",

				"            4            1  31.2499E+00  2.52933E+01  5.46357E+01  3.69109E+01",
				"            4            2  32.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            4            3  33.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",

				"            5            1  51.2691E+01  0.00000E+00  0.00000E+00  0.00000E+00",
				"            5            2  52.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            5            3  53.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",

				"            6            1  61.9795E+00  2.56533E+01  5.45618E+01  3.71115E+01",
				"            6            2  62.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            6            3  63.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",

				"            7            1  71.0000E+01  7.90000E+01  7.90000E+01  7.90000E+01",
				"            7            2  72.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            7            3  73.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",

				"            8            1  81.5368E+00  4.41891E+01  7.94208E+01  6.01976E+01",
				"            8            2  82.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            8            3  83.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",

				"            9            1  91.4166E+00  4.47258E+01  7.93537E+01  6.04504E+01",
				"            9            2  92.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            9            3  93.0000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",

				"            10           1  101.776E+01  0.00000E+00  0.00000E+00  0.00000E+00",
				"            10           2  102.000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
				"            10           3  103.000E-00 -3.91460E-03 -8.00654E-03 -1.95727E-03",
			},
			context:  "subpop",
			etaCount: 4,
			epsCount: 1,
		},
	}

	for _, tt := range tests {
		ed := ParseShkLines(tt.lines)

		shk := ParseShrinkage(ed.EstimationLines[0], tt.etaCount, tt.epsCount)
		assert.Equal(t, 3, len(shk), "Fail :"+tt.context)

		for i := range shk {
			assert.Equal(t, int64(i+1), shk[i].SubPop, "Fail :"+tt.context)
		}

		sd := ParseShkData(ed, tt.etaCount, tt.epsCount)
		assert.Equal(t, float64(11), sd[0][0].EtaBar[0], "Fail :"+tt.context)
		assert.Equal(t, float64(92), sd[0][1].EbvVR[0], "Fail :"+tt.context)
		assert.Equal(t, float64(33), sd[0][2].EtaSD[0], "Fail :"+tt.context)
	}
}
