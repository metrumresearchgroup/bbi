package nonmemutils

import "testing"

var omegaResults01 = []string{
	"            ETA1      ETA2",
	"",
	" ETA1",
	"+        1.23E-01",
	"",
	" ETA2",
	"+        0.00E+00  1.54E-01",
}

var omegaResults01Parsed = []string{
	"1.23E-01",
	"0.00E+00",
	"1.54E-01",
}

func TestParseOmegaResultsBlock(t *testing.T) {

	parsedData := ParseOmegaResults(omegaResults01)
	for i, val := range parsedData {
		if val != omegaResults01Parsed[i] {
			t.Log("GOT: ", val, " EXPECTED: ", omegaResults01Parsed[i])
			t.Fail()
		}
	}
}
