package parser

import (
	"bufio"
	"os"
	"testing"
)

// TODO: remove uglyString if we find no use
var _ /*uglyString*/ = []string{
	"$THETA (0,2720) ; CL",
	"$THETA(0,2650) ; V2",
	" $THETA (0,7730) ; V3",
	" (0,3410)",
	" $THETA(0,0.232) FIX ; KA transit",
	" 0.75 FIX ; allo-WT",
}

// TODO: remove ratThetaBlock02 if we find no use
var _ /*rawThetaBlock02*/ = []string{
	"$THETA  (0,5) ; CL",
	"(0,97.2) ; V",
	"(0,7.47,100) ; Q",
	"(0,106,500) ; V2",
	"$THETA  (-0.011,0.00977,0.030) ; CLWT1",
	"$THETA  (-1,-0.16,5) ; V1SEX1",
	"$THETA  (-0.011,0.0142,0.030) ; V1WT1",
	"$THETA  (-1000000,0.0178,0.045) ; CLCRCL1",
	"(-0.038,0.00231,1000000) ; CLCRCL2",
}

// TODO: remove rawThetaBlock02Cleaned if we find no use
var _ /*rawThetaBlock02Cleaned*/ = []string{
	"(0,5) ; CL",
	"(0,97.2) ; V",
	"(0,7.47,100) ; Q",
	"(0,106,500) ; V2",
	"(-0.011,0.00977,0.030) ; CLWT1",
	"(-1,-0.16,5) ; V1SEX1",
	"(-0.011,0.0142,0.030) ; V1WT1",
	"(-1000000,0.0178,0.045) ; CLCRCL1",
	"(-0.038,0.00231,1000000) ; CLCRCL2",
}

func readLines(path string) ([]string, error) {
	inFile, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer inFile.Close()
	scanner := bufio.NewScanner(inFile)
	scanner.Split(bufio.ScanLines)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	return lines, nil
}

var thetaSliceCleanedResult = []string{
	"(0,2720) ; CL",
	"(0,2650) ; V2",
	"(0,7730) ; V3",
	"(0,3410) ; CL2",
	"(0,0.232) FIX ; KA transit",
	"0.75 FIX ; allo-WT",
}

func TestCleaningThetaBlock01(t *testing.T) {
	testData, err := readLines("testdata/blocks/theta-block-01.lst")
	if err != nil {
		t.Log("could not read testData with err: ", err)
		t.Fail()
	}
	parsedData := CleanThetaBlock(testData)
	if len(parsedData) != len(thetaSliceCleanedResult) {
		t.Log("Mismatch between number of rows of cleaned and correct results")
		// failNow as don't want to go to next check as go will panic since loops won't match
		t.FailNow()
	}
	for i, val := range parsedData {
		if val != thetaSliceCleanedResult[i] {
			t.Log("GOT: ", val, " EXPECTED: ", thetaSliceCleanedResult[i])
			t.Fail()
		}
	}
}

func TestCleaningThetaBlock02(t *testing.T) {
	testData, err := readLines("testdata/blocks/theta-block-02.lst")
	if err != nil {
		t.Log("could not read testData with err: ", err)
		t.Fail()
	}
	parsedData := CleanThetaBlock(testData)
	if len(parsedData) != len(thetaSliceCleanedResult) {
		t.Log("Mismatch between number of rows of cleaned and correct results")
		// failNow as don't want to go to next check as go will panic since loops won't match
		t.FailNow()
	}
	for i, val := range parsedData {
		if val != thetaSliceCleanedResult[i] {
			t.Log("GOT: ", val, " EXPECTED: ", thetaSliceCleanedResult[i])
			t.Fail()
		}
	}
}

func TestCleaningThetaBlock03(t *testing.T) {
	testData, err := readLines("testdata/blocks/theta-block-03.lst")
	if err != nil {
		t.Log("could not read testData with err: ", err)
		t.Fail()
	}

	parsedData := CleanThetaBlock(testData)
	if len(parsedData) != len(thetaSliceCleanedResult) {
		t.Log("Mismatch between number of rows of cleaned and correct results")
		// failNow as don't want to go to next check as go will panic since loops won't match
		t.FailNow()
	}
	for i, val := range parsedData {
		if val != thetaSliceCleanedResult[i] {
			t.Log("GOT: ", val, " EXPECTED: ", thetaSliceCleanedResult[i])
			t.Fail()
		}
	}
}
