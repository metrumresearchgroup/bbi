package parser

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

var omegaResults02 = []string{
	"            ETA1      ETA2",
	"",
	" ETA1",
	"+        1.23E-01",
	"",
	" ETA2",
	"+        .........  1.54E-01",
}

var omegaResults02Parsed = []string{
	"1.23E-01",
	".........",
	"1.54E-01",
}

func TestParseBlockResults(t *testing.T) {

	parsedData := ParseBlockResults(omegaResults01)
	for i, val := range parsedData {
		if val != omegaResults01Parsed[i] {
			t.Log("GOT: ", val, " EXPECTED: ", omegaResults01Parsed[i])
			t.Fail()
		}
	}
}

func TestParseBlockResultsWithDots(t *testing.T) {

	parsedData := ParseBlockResults(omegaResults02)
	for i, val := range parsedData {
		if val != omegaResults02Parsed[i] {
			t.Log("GOT: ", val, " EXPECTED: ", omegaResults02Parsed[i])
			t.Fail()
		}
	}
}
