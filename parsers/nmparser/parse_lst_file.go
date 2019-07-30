package parser

import (
	"strconv"
	"strings"
)

// LstData is the output struct from a lst file
type LstData struct {
	RunDetails              RunDetails
	FinalParameterEstimates FinalParameterEstimates
	FinalParameterStdErr    FinalParameterEstimates
	ParameterStructures     ParameterStructures
	ParameterNames          ParameterNames
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
	ofvIndex := 0
	startParameterStructuresIndex := 0
	endParameterStucturesIndex := 0
	finalParameterEstimatesIndex := 0
	standardErrorEstimateIndex := 0
	covarianceMatrixEstimateIndex := 0
	startThetaIndex := 0
	endSigmaIndex := 0

	for i, line := range lines {
		switch {
		case strings.Contains(line, "$THETA") && startThetaIndex == 0:
			startThetaIndex = i
		case strings.Contains(line, "$EST") && endSigmaIndex == 0:
			endSigmaIndex = i
		case strings.Contains(line, "0LENGTH OF THETA"):
			startParameterStructuresIndex = i
		case strings.Contains(line, "0DEFAULT SIGMA BOUNDARY"):
			endParameterStucturesIndex = i
		case strings.Contains(line, "#OBJV"):
			ofvIndex = i
		case strings.Contains(line, "FINAL PARAMETER ESTIMATE"):
			// want to go 3 more lines to get into text not labelled block
			finalParameterEstimatesIndex = i + 3
		case strings.Contains(line, "STANDARD ERROR OF ESTIMATE"):
			standardErrorEstimateIndex = i + 3
		case strings.Contains(line, "COVARIANCE MATRIX OF ESTIMATE"):
			// only want to set this the first time it is detected
			// another block called "INVERSE COVARIANCE ...." will match this
			if covarianceMatrixEstimateIndex == 0 {
				covarianceMatrixEstimateIndex = i + 3
			}
		default:
			continue
		}
	}

	var finalParameterEst FinalParameterEstimates
	var finalParameterStdErr FinalParameterEstimates
	var parameterStructures ParameterStructures
	var parameterNames ParameterNames

	if standardErrorEstimateIndex > finalParameterEstimatesIndex {
		finalParameterEst = ParseFinalParameterEstimates(lines[finalParameterEstimatesIndex:standardErrorEstimateIndex])
	}

	if covarianceMatrixEstimateIndex > standardErrorEstimateIndex {
		finalParameterStdErr = ParseFinalParameterEstimates(lines[standardErrorEstimateIndex:covarianceMatrixEstimateIndex])
	}

	if (endParameterStucturesIndex + 1) > startParameterStructuresIndex {
		parameterStructures = ParseParameterStructures(lines[startParameterStructuresIndex : endParameterStucturesIndex+1])
	}

	if endSigmaIndex > startThetaIndex {
		parameterNames = ParseParameterNames(lines[startThetaIndex:endSigmaIndex])
	}

	result := LstData{
		ParseRunDetails(lines),
		finalParameterEst,
		finalParameterStdErr,
		parameterStructures,
		parameterNames,
		parseOFV(lines[ofvIndex]),
	}
	return result
}
