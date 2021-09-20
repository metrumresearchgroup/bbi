package parser

import (
	"strings"
	"testing"

	"github.com/metrumresearchgroup/wrapt"

	"github.com/metrumresearchgroup/bbi/utils"
)

func TestReadParseGrdLines(tt *testing.T) {
	var tests = []struct {
		lines    []string
		expected bool
		status   bool
		context  string
	}{
		{
			lines: []string{
				"TABLE NO.     1: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)       GRD(10)      GRD(11)",
				"		  15  1.01544E-02  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03 -5.86988E-03 1.83069E-01  8.36940E-01  0.00000E+00",
			},
			expected: true,
			status:   true,
			context:  "final zero",
		},
		{
			lines: []string{
				"TABLE NO.     1: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)",
				"		   0 -3.85613E+01  4.33258E+01  2.33602E+02  2.12304E+02  2.12964E+02 -5.05978E+00  2.86950E+01 -1.58137E+01  6.81739E+01",
				"   	   5  8.77273E+01  1.21734E+02 -7.66232E+01 -3.84937E+01  1.10997E+01  1.49415E+00 -7.95547E-01 -7.83447E+00 -1.75762E+01",
				"		  10  4.84466E+01  5.77126E+01 -3.75199E+01 -9.45384E+00 -3.79759E+00  6.11768E+00 -3.10524E+00  3.68255E+00 -2.02148E+01",
				"		  15  1.01544E-02  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03 -5.86988E-03",
				"",
				"TABLE NO.     2: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)",
				"		   0 -3.85613E+01  4.33258E+01  2.33602E+02  2.12304E+02  2.12964E+02 -5.05978E+00  2.86950E+01 -1.58137E+01  6.81739E+01",
				"   	   5  8.77273E+01  1.21734E+02 -7.66232E+01 -3.84937E+01  1.10997E+01  1.49415E+00 -7.95547E-01 -7.83447E+00 -1.75762E+01",
				"		  10  4.84466E+01  5.77126E+01 -3.75199E+01 -9.45384E+00 -3.79759E+00  6.11768E+00 -3.10524E+00  3.68255E+00 -2.02148E+01",
				"		  15  1.01544E-02  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03 -5.86988E-03",
				"",
				"TABLE NO.     3: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)",
				"		   0 -3.85613E+01  4.33258E+01  2.33602E+02  2.12304E+02  2.12964E+02 -5.05978E+00  2.86950E+01 -1.58137E+01  6.81739E+01",
				"   	   5  8.77273E+01  1.21734E+02 -7.66232E+01 -3.84937E+01  1.10997E+01  1.49415E+00 -7.95547E-01 -7.83447E+00 -1.75762E+01",
				"		  10  4.84466E+01  5.77126E+01 -3.75199E+01 -9.45384E+00 -3.79759E+00  6.11768E+00 -3.10524E+00  3.68255E+00 -2.02148E+01",
				"		  15  1.01544E-02  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03 -5.86988E-03",
			},
			expected: false,
			status:   false,
			context:  "no zero",
		},
		{
			lines: []string{
				"TABLE NO.     1: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)",
				"		   0 -3.85613E+01  4.33258E+01  2.33602E+02  2.12304E+02  2.12964E+02 -5.05978E+00  2.86950E+01 -1.58137E+01  6.81739E+01",
				"   	   5  8.77273E+01  1.21734E+02 -7.66232E+01 -3.84937E+01  1.10997E+01  1.49415E+00 -7.95547E-01 -7.83447E+00 -1.75762E+01",
				"		  10  4.84466E+01  5.77126E+01 -3.75199E+01 -9.45384E+00 -3.79759E+00  6.11768E+00 -3.10524E+00  3.68255E+00 -2.02148E+01",
				"		  15  1.01544E-02  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03 -5.86988E-03",
				"",
				"TABLE NO.     2: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)",
				"		   0 -3.85613E+01  4.33258E+01  2.33602E+02  2.12304E+02  2.12964E+02 -5.05978E+00  2.86950E+01 -1.58137E+01  6.81739E+01",
				"   	   5  0.000000000  1.21734E+02 -7.66232E+01 -3.84937E+01  1.10997E+01  1.49415E+00 -7.95547E-01 -7.83447E+00 -1.75762E+01",
				"		  10  4.84466E+01  5.77126E+01 -3.75199E+01 -9.45384E+00 -3.79759E+00  6.11768E+00 -3.10524E+00  3.68255E+00 -2.02148E+01",
				"		  15  1.01544E-02  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03 -5.86988E-03",
				"",
				"TABLE NO.     3: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)",
				"		   0 -3.85613E+01  4.33258E+01  2.33602E+02  2.12304E+02  2.12964E+02 -5.05978E+00  2.86950E+01 -1.58137E+01  6.81739E+01",
				"   	   5  8.77273E+01  1.21734E+02 -7.66232E+01 -3.84937E+01  1.10997E+01  1.49415E+00 -7.95547E-01 -7.83447E+00 -1.75762E+01",
				"		  10  4.84466E+01  5.77126E+01 -3.75199E+01 -9.45384E+00 -3.79759E+00  6.11768E+00 -3.10524E+00  3.68255E+00 -2.02148E+01",
				"		  15  1.01544E-02  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03 -5.86988E-03",
			},
			expected: false,
			status:   false,
			context:  "zero, not final",
		},
		{
			lines: []string{
				"TABLE NO.     1: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)",
				"		   0 -3.85613E+01  4.33258E+01  2.33602E+02  2.12304E+02  2.12964E+02 -5.05978E+00  2.86950E+01 -1.58137E+01  6.81739E+01",
				"   	   5  8.77273E+01  1.21734E+02 -7.66232E+01 -3.84937E+01  1.10997E+01  1.49415E+00 -7.95547E-01 -7.83447E+00 -1.75762E+01",
				"		  10  4.84466E+01  5.77126E+01 -3.75199E+01 -9.45384E+00 -3.79759E+00  6.11768E+00 -3.10524E+00  3.68255E+00 -2.02148E+01",
				"		  15  1.01544E-02  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03 -5.86988E-03",
				"",
				"TABLE NO.     2: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)",
				"		   0 -3.85613E+01  4.33258E+01  2.33602E+02  2.12304E+02  2.12964E+02 -5.05978E+00  2.86950E+01 -1.58137E+01  6.81739E+01",
				"   	   5  8.77273E+01  1.21734E+02 -7.66232E+01 -3.84937E+01  1.10997E+01  1.49415E+00 -7.95547E-01 -7.83447E+00 -1.75762E+01",
				"		  10  4.84466E+01  5.77126E+01 -3.75199E+01 -9.45384E+00 -3.79759E+00  6.11768E+00 -3.10524E+00  3.68255E+00 -2.02148E+01",
				"		  15  1.01544E-02  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03 -5.86988E-03",
				"",
				"TABLE NO.     3: First Order Conditional Estimation with Interaction: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
				" ITERATION    GRD(1)       GRD(2)       GRD(3)       GRD(4)       GRD(5)       GRD(6)       GRD(7)       GRD(8)       GRD(9)",
				"		   0 -3.85613E+01  4.33258E+01  2.33602E+02  2.12304E+02  2.12964E+02 -5.05978E+00  2.86950E+01 -1.58137E+01  6.81739E+01",
				"   	   5  8.77273E+01  1.21734E+02 -7.66232E+01 -3.84937E+01  1.10997E+01  1.49415E+00 -7.95547E-01 -7.83447E+00 -1.75762E+01",
				"		  10  4.84466E+01  5.77126E+01 -3.75199E+01 -9.45384E+00 -3.79759E+00  6.11768E+00 -3.10524E+00  3.68255E+00 -2.02148E+01",
				"		  15  1.000000000  1.25236E-03  1.93608E-02 -6.25415E-02  4.45540E-02 -1.47003E-02 -2.31826E-02 -5.90237E-03  0.00000E+00",
			},
			expected: true,
			status:   false,
			context:  "final zero",
		},
	}

	for _, test := range tests {
		tt.Run(test.context, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			extData := ParseGrdLines(test.lines)
			t.R.Equal(test.lines[0], extData.EstimationMethods[0], "Fail :"+test.context)
			t.R.Equal("GRD(1)", extData.ParameterNames[1], "Fail :"+test.context)
			t.R.Equal(strings.Trim(test.lines[2], "\t "), extData.EstimationLines[0][0], "Fail :"+test.context)

			parametersData, parameterNames := ParseGrdData(extData)
			t.R.Equal(test.lines[0], parametersData[0].Method, "Fail :"+test.context)
			t.R.Equal("GRD(1)", parameterNames.Theta[0], "Fail :"+test.context)

			hasZero := utils.HasZero(parametersData[len(parametersData)-1].Fixed.Theta)
			t.R.Equal(test.expected, hasZero, "Fail :"+test.context)

			hasZeroGradient := HasZeroGradient(parametersData[len(parametersData)-1].Fixed.Theta)
			t.R.Equal(test.status, hasZeroGradient, "Fail :"+test.context)
		})
	}
}
