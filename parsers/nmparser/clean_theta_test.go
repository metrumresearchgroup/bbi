package parser

import (
	"bufio"
	"os"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

// TODO: remove uglyString if we find no use...
var _ = /*uglyString*/ []string{
	"$THETA (0,2720) ; CL",
	"$THETA(0,2650) ; V2",
	" $THETA (0,7730) ; V3",
	" (0,3410)",
	" $THETA(0,0.232) FIX ; KA transit",
	" 0.75 FIX ; allo-WT",
}

// TODO: remove ratThetaBlock02 if we find no use...
var _ = /*rawThetaBlock02*/ []string{
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

// TODO: remove rawThetaBlock02Cleaned if we find no use...
var _ = /*rawThetaBlock02Cleaned*/ []string{
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

func TestCleaningThetaBlock(tt *testing.T) {
	tests := []struct {
		name     string
		filename string
	}{
		{
			name:     "block-01",
			filename: "testdata/blocks/theta-block-01.lst",
		},
		{
			name:     "block-02",
			filename: "testdata/blocks/theta-block-02.lst",
		},
		{
			name:     "block-03",
			filename: "testdata/blocks/theta-block-03.lst",
		},
	}
	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			testData, err := readLines(test.filename)
			t.R.NoError(err, "reading test data")

			parsedData := CleanThetaBlock(testData)

			t.R.Equal(thetaSliceCleanedResult, parsedData)
		})
	}
}
