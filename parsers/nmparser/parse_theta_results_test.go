package parser

import (
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestParseThetaResultsBlock(tt *testing.T) {
	var thetaResults01 = []string{
		"",
		"",
		"         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      TH 9      TH10      TH11      TH12",
		"          TH13",
		"",
		"         2.69E+03  2.43E+03  7.80E+03  3.45E+03  1.80E-01  2.13E+00  4.14E-01  2.29E-02  2.50E+00  1.00E-03  8.33E-01  1.55E+00",
		"          1.13E+00",
		"",
		"",
		"",
	}

	var thetaResults01Parsed = []float64{
		2690,
		2430,
		7800,
		3450,
		0.180,
		2.13,
		0.414,
		0.0229,
		2.5,
		0.001,
		0.833,
		1.55,
		1.13,
	}

	parsedData := ParseThetaResults(thetaResults01)

	testId := "UNIT-NMP-010"
	tt.Run(testId, func(tt *testing.T) {
		t := wrapt.WrapT(tt)
		t.R.Equal(thetaResults01Parsed, parsedData)
	})
}
