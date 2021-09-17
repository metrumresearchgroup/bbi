package parser

import (
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestParseFinalParameterEstimatesFromLst(tt *testing.T) {
	tests := []struct {
		name  string
		input []string
		want  ParametersResult
	}{
		{
			name: "final parameter estimate",
			input: []string{
				"********************                             FINAL PARAMETER ESTIMATE                           ********************",
				" ********************                                                                                ********************",
				" ************************************************************************************************************************",
				" ",
				"",
				" THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********",
				"",
				"         TH 1      TH 2      TH 3      TH 4     ",
				" ",
				"         4.79E+00  9.02E+01  7.47E+00  1.05E+02",
				" ",
				"",
				" OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********",
				"",
				"            ETA1      ETA2   ",
				" ",
				" ETA1",
				"+        1.58E-01",
				" ",
				" ETA2",
				"+        1.22E-01  1.33E-01",
				" ",
				"",
				" SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****",
				"",
				"            EPS1      EPS2   ",
				" ",
				" EPS1",
				"+        1.45E+03",
				" ",
				" EPS2",
				"+        0.00E+00  7.39E-03",
				" ",
				"1",
				"",
				" OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******",
				"",
				"            ETA1      ETA2   ",
				" ",
				" ETA1",
				"+        3.98E-01",
				" ",
				" ETA2",
				"+        8.42E-01  3.65E-01",
				" ",
				"",
				" SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***",
				"",
				"            EPS1      EPS2   ",
				" ",
				" EPS1",
				"+        3.81E+01",
				" ",
				" EPS2",
				"+        0.00E+00  8.60E-02",
				" ",
				"1",
			},
			want: ParametersResult{
				Theta: []float64{4.79, 90.2, 7.47, 105},
				Omega: []float64{0.158, 0.122, 0.133},
				Sigma: []float64{1450, 0, 0.00739},
			},
		},
		{
			name: "sigma",
			input: []string{
				"	SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****",
				"",
				"",
				"      EPS1",
				"",
				"EPS1",
				"+        2.45E-03",
				"",
				"1",
				"",
				"",
				"OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******",
				"",
				"",
				"	ETA1      ETA2      ETA3     ",
				"",
				"ETA1",
				"+        3.17E-01",
				"",
				"ETA2",
				"+        0.00E+00  1.90E-01",
				"",
				"ETA3",
				"+        0.00E+00  0.00E+00  1.06E-01",
				"",
				"",
			},

			// the original test contained only the sigma value, but this
			// parse results in only omega value being parsed.
			want: ParametersResult{
				Sigma: []float64{
					0.00245,
				},
			},
		},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got := ParseFinalParameterEstimatesFromLst(test.input)

			t.R.Equal(test.want, got)
		})
	}
}
