package parser

import (
	"strconv"
	"strings"
)

// LstData is the output struct from a lst file
type LstData struct {
	FinalParameterEstimates FinalParameterEstimates
	OFV                     float64
}

func parseOFV(line string) float64 {
	// get rid of everything but stars and ofv
	result := line[10:]
	result = strings.Replace(result, "*", "", -1)
	output, _ := strconv.ParseFloat(strings.Fields(result)[0], 64)
	return output
}

// ParseLstEstimationFile parses the lst file
func ParseLstEstimationFile(lines []string) LstData {

	var ofvLine int
	var finalParameterEstimatesLine int
	var standardErrorEstimateLine int

	for i, line := range lines {
		switch {
		case strings.Contains(line, "#OBJV"):
			ofvLine = i
		case strings.Contains(line, "FINAL PARAMETER ESTIMATE"):
			// want to go 3 more lines to get into text not labelled block
			finalParameterEstimatesLine = i + 3
		case strings.Contains(line, "STANDARD ERROR OF ESTIMATE"):
			standardErrorEstimateLine = i + 3
		}

	}
	result := LstData{
		ParseFinalParameterEstimates(lines[finalParameterEstimatesLine:standardErrorEstimateLine]),
		parseOFV(lines[ofvLine]),
	}
	return result
}
