package parser

import (
	"reflect"
	"testing"
)

var RunDetails01 = []string{
	"Days until program expires : 122",
	"1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.2.0",
	" ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN",
	" #TERM:",
	"0MINIMIZATION SUCCESSFUL",
	" NO. OF FUNCTION EVALUATIONS USED:      352",
	" NO. OF SIG. DIGITS IN FINAL EST.:  3.4",
	"",
	"#TERE:",
	"Elapsed estimation time in seconds:     6.84",
	"Elapsed covariance time in seconds:     3.34",
	"This file was created using /opt/NONMEM/nm72g/run/nmfe72",
	"Started  Tue Dec 17 18:10:55 2013",
	"Finished Tue Dec 17 18:11:32 2013",
}

var RunDetails01Results = RunDetails{
	"7.2.0",
	"Tue Dec 17 18:10:55 2013",
	"Tue Dec 17 18:11:32 2013",
	6.84,
	3.34,
	352,
	3.4,
}

func TestParseRunDetails(t *testing.T) {
	parsedData := ParseRunDetails(RunDetails01)
	if !reflect.DeepEqual(parsedData, RunDetails01Results) {
		t.Log("GOT: ", parsedData, " Expected: ", RunDetails01Results)
		t.Fail()
	}
}
