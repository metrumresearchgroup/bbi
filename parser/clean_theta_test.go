package parser

import (
	"bufio"
	"os"
	"testing"
)

var uglyString = []string{
	"$THETA (0,2720) ; CL",
	"$THETA(0,2650) ; V2",
	" $THETA (0,7730) ; V3",
	" (0,3410)",
	" $THETA(0,0.232) FIX ; KA transit",
	" 0.75 FIX ; allo-WT",
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
	testData, _ := readLines("fixtures/blocks/theta-block-01.lst")
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
	testData, _ := readLines("fixtures/blocks/theta-block-02.lst")
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
	testData, _ := readLines("fixtures/blocks/theta-block-03.lst")
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
