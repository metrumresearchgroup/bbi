package parser

import (
	"reflect"
	"strings"
	"testing"

	"github.com/spf13/afero"
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
	"$PROB 3.mod, double inital estimates",
	"", // TODO, pass full path control stream file name into ParseRunDetails
	// 3.mod; initial estimate of inter-subject variability (matrix). also ETA
	"#METH: First Order Conditional Estimation with Interaction",
	"$DATA ../../derived/mock1.csv IGNORE=C",
	"TOT. NO. OF INDIVIDUALS:       50",
	"TOT. NO. OF OBS RECS:      442",
	"NO. OF DATA RECS IN DATA SET:      492",
	"$TABLE NOPRINT ONEHEADER FILE=./1.tab",
}

var RunDetails01Results = RunDetails{
	"7.2.0",
	"Tue Dec 17 18:10:55 2013",
	"Tue Dec 17 18:11:32 2013",
	6.84,
	3.34,
	352,
	3.4,
	"3.mod, double inital estimates",
	"",
	[]string{"First Order Conditional Estimation with Interaction"},
	"../../derived/mock1.csv",
	50,
	442,
	492,
	[]string{"./1.tab"},
}

var RunDetails02 = "../../testdata/2.lst"

var RunDetails02Results = RunDetails{
	"7.4.3",
	"Fri Jul 12 09:27:14 EDT 2019",
	"Fri Jul 12 09:27:20 EDT 2019",
	0.68,
	0.02,
	178,
	3.1,
	"1 model, 1 comp",
	"",
	[]string{"First Order Conditional Estimation with Interaction"},
	"../../derived/mock1.csv",
	50,
	442,
	492,
	[]string{"./1.tab"},
}

func TestParseRunDetails(t *testing.T) {
	parsedData := ParseRunDetails(RunDetails01)
	if !reflect.DeepEqual(parsedData, RunDetails01Results) {
		t.Log("\nGOT: ", parsedData, "\n Expected: ", RunDetails01Results)
		t.Fail()
	}
}

func TestParseRunDetailsFromFile(t *testing.T) {
	OsFs := afero.NewOsFs()
	var runDetails02, _ = afero.ReadFile(OsFs, RunDetails02)
	lines := strings.Split(string(runDetails02), "\n")
	parsedData := ParseRunDetails(lines)
	if !reflect.DeepEqual(parsedData, RunDetails02Results) {
		t.Log("\nGOT: ", parsedData, "\n Expected: ", RunDetails02Results)
		t.Fail()
	}
}

func TestGetLines(t *testing.T) {
	name := "Positive count 1, index 0"
	index := 0
	count := 1
	lines := getLines(RunDetails01, index, count)
	for i := 0; i < count; i++ {
		if lines[i] != RunDetails01[i] {
			t.Log("\nFAILED: ", name)
			t.Log("\nGOT: ", lines[i], "\n Expected: ", RunDetails01[i])
			t.Fail()
		}
	}
	name = "Positive count 5, index 5"
	index = 5
	count = 5
	lines = getLines(RunDetails01, index, count)
	for i := 0; i < count; i++ {
		if lines[i] != RunDetails01[index+i] {
			t.Log("\nFAILED: ", name)
			t.Log("\nGOT: ", lines[i], "\n Expected: ", RunDetails01[index+i])
			t.Fail()
		}
	}

	name = "Positive count 10, index 99"
	index = 99
	count = 10
	lines = getLines(RunDetails01, index, count)
	if len(lines) > 0 {
		t.Log("\nFAILED: ", name)
		t.Log("\nGOT: ", len(lines), "\n Expected: 0")
		t.Fail()
	}

	name = "Positive count 17, index 10"
	index = 17
	count = 10
	lines = getLines(RunDetails01, index, count)
	if len(lines) != 5 {
		t.Log("\nFAILED: ", name)
		t.Log("\nGOT: ", len(lines), "\n Expected: 5")
		t.Fail()
	}

	name = "Negative count -3, index 21"
	index = 21
	count = -3
	lines = getLines(RunDetails01, index, count)
	for i := 0; i < (count * -1); i++ {
		if lines[i] != RunDetails01[index+count+i+1] {
			t.Log("\nFAILED: ", name)
			t.Log("\nGOT: ", lines[i], "\n Expected: ", RunDetails01[index+i])
			t.Fail()
		}
	}

	name = "Negative count -10, index 5"
	index = 5
	count = -10
	lines = getLines(RunDetails01, index, count)
	if len(lines) != 6 {
		t.Log("\nFAILED: ", name)
		t.Log("\nGOT: ", len(lines), "\n Expected: 5")
		t.Fail()
	}
}
