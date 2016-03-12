package nonmemutils

import "testing"

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

var thetaResults01Parsed = []string{
	"2.69E+03",
	"2.43E+03",
	"7.80E+03",
	"3.45E+03",
	"1.80E-01",
	"2.13E+00",
	"4.14E-01",
	"2.29E-02",
	"2.50E+00",
	"1.00E-03",
	"8.33E-01",
	"1.55E+00",
	"1.13E+00",
}

func TestParseThetaResultsBlock(t *testing.T) {

	parsedData := ParseThetaResults(thetaResults01)
	for i, val := range parsedData {
		if val != thetaResults01Parsed[i] {
			t.Log("GOT: ", val, " EXPECTED: ", thetaResults01Parsed[i])
			t.Fail()
		}
	}
}
