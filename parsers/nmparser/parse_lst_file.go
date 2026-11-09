package parser

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/metrumresearchgroup/bbi/utils"
)

func parseGradient(lines []string) bool {
	if len(lines) == 0 {
		return false
	}

	fields := strings.Fields(strings.TrimSpace(strings.ReplaceAll(lines[len(lines)-1], "GRADIENT:", "")))
	if len(fields) > 0 {
		result := make([]float64, len(fields))
		for i, val := range fields {
			n := strToFloat(val)
			result[i] = n
		}

		return utils.HasZero(result)
	}

	return false
}

// get gradient lines until a blank line is reached.
func getGradientLine(lines []string, start int) string {
	var sb strings.Builder
	sb.WriteString(lines[start])
	for _, line := range lines[start+1:] {
		if line == "" {
			break
		}
		sb.WriteString(line)
	}

	return sb.String()
}

// ParseLstEstimationFile parses the lst file.
func ParseLstEstimationFile(lines []string) SummaryOutput {
	runHeuristics := NewRunHeuristics()
	var allOfvDetails []OfvDetails
	var allCondDetails []ConditionNumDetails
	var finalParameterEstimatesIndex int
	var standardErrorEstimateIndex int
	var covarianceMatrixEstimateIndex int
	var startThetaIndex int
	var endSigmaIndex int
	var gradientLines []string

	for i, line := range lines {
		switch {
		case strings.Contains(line, "$THETA") && startThetaIndex == 0:
			fields := strings.Fields(line)
			for _, field := range fields {
				// can also have $THETAI/$THETAR/$THETAPV
				if field == "$THETA" {
					startThetaIndex = i
				}
			}
		case strings.Contains(line, "$EST") && endSigmaIndex == 0:
			endSigmaIndex = i
		case strings.Contains(line, "#METH"):
			// starting new estimation method, make new details objects
			method := strings.TrimSpace(strings.ReplaceAll(line, "#METH:", ""))
			allOfvDetails = append(allOfvDetails, NewOfvDetails(method))
			allCondDetails = append(allCondDetails, NewConditionNumDetails(method, DefaultFloat64))
		case strings.Contains(line, "#OBJV"):
			allOfvDetails = parseOFV(line, allOfvDetails)
		case strings.Contains(line, "CONSTANT TO OBJECTIVE FUNCTION"):
			allOfvDetails = parseOFV(line, allOfvDetails)
		case strings.Contains(line, "OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT"):
			allOfvDetails = parseOFV(line, allOfvDetails)
		case strings.Contains(line, "OBJECTIVE FUNCTION VALUE WITH CONSTANT"):
			allOfvDetails = parseOFV(line, allOfvDetails)
		case strings.Contains(line, "FINAL PARAMETER ESTIMATE"):
			// want to go 3 more lines to get into text not labelled block
			finalParameterEstimatesIndex = i + 3
		case strings.Contains(line, "STANDARD ERROR OF ESTIMATE"):
			standardErrorEstimateIndex = i + 3
		case strings.Contains(line, "COVARIANCE MATRIX OF ESTIMATE"):
			// only want to set this the first time it is detected
			// another block called "INVERSE COVARIANCE ...." will match this
			if !strings.Contains(line, "INVERSE") {
				covarianceMatrixEstimateIndex = i + 3
			}
		case strings.Contains(line, "0MINIMIZATION TERMINATED"):
			runHeuristics.MinimizationTerminated = true
		case strings.Contains(line, "GRADIENT:"):
			gradientLines = append(gradientLines, getGradientLine(lines, i))
		case strings.Contains(line, "RESET HESSIAN"):
			runHeuristics.HessianReset = true
		case strings.Contains(line, "PARAMETER ESTIMATE IS NEAR ITS BOUNDARY"):
			runHeuristics.ParameterNearBoundary = true
		case strings.Contains(line, "COVARIANCE STEP ABORTED"):
			runHeuristics.CovarianceStepAborted = true
		case strings.Contains(line, "Forcing positive definiteness"):
			runHeuristics.EigenvalueIssues = true
		case strings.Contains(line, "EIGENVALUES OF COR MATRIX OF ESTIMATE"):
			allCondDetails = parseConditionNumberLst(lines, i, allCondDetails)
		default:
			continue
		}
	}

	runHeuristics.HasFinalZeroGradient = parseGradient(gradientLines)
	runHeuristics.LargeConditionNumber = getLargeConditionNumber(allCondDetails)
	for _, cd := range allCondDetails {
		if (cd.ConditionNumber <= 0) && (cd.ConditionNumber != DefaultFloat64) {
			runHeuristics.EigenvalueIssues = true
		}
	}

	var finalParameterEst ParametersResult
	var finalParameterStdErr ParametersResult
	// var parameterNames ParameterNames

	if standardErrorEstimateIndex > finalParameterEstimatesIndex {
		finalParameterEst = ParseFinalParameterEstimatesFromLst(lines[finalParameterEstimatesIndex:standardErrorEstimateIndex])
	}

	// if covarianceMatrixEstimateIndex > 0 {
	// 	covTheta = getThetaValues(lines, covarianceMatrixEstimateIndex)
	// }

	// if correlationMatrixEstimateIndex > 0 {
	// 	corTheta = getThetaValues(lines, correlationMatrixEstimateIndex)
	// }

	if covarianceMatrixEstimateIndex > standardErrorEstimateIndex {
		finalParameterStdErr = ParseFinalParameterEstimatesFromLst(lines[standardErrorEstimateIndex:covarianceMatrixEstimateIndex])
	}

	// TODO: replace parsing parameter names
	// if endSigmaIndex > startThetaIndex {
	// 	parameterNames = ParseParameterNames(lines[startThetaIndex:endSigmaIndex])
	// }
	// TODO re-replace parameter data from lst
	result := SummaryOutput{
		RunHeuristics: runHeuristics,
		RunDetails:    ParseRunDetails(lines),
		ParametersData: []ParametersData{
			ParametersData{
				Estimates: finalParameterEst,
				StdErr:    finalParameterStdErr,
			},
		},
		ParameterNames: NewDefaultParameterNames(len(finalParameterEst.Theta), len(finalParameterEst.Omega), len(finalParameterEst.Sigma)),

		OFV: allOfvDetails,

		ConditionNumber: allCondDetails,
	}

	return result
}

func parseOFV(line string, allOfvDetails []OfvDetails) []OfvDetails {
	// always modify the most recently created OfvDetails
	ofvDetails := &allOfvDetails[len(allOfvDetails)-1]

	if strings.Contains(line, "#OBJV:") {
		result := strings.ReplaceAll(line, "*", "")
		ofvDetails.OFVNoConstant = strToFloat(
			strings.TrimSpace(strings.ReplaceAll(result, "#OBJV:", "")))
	} else if strings.Contains(line, "CONSTANT TO OBJECTIVE FUNCTION") {
		constantString := strings.TrimSpace(strings.ReplaceAll(line, "CONSTANT TO OBJECTIVE FUNCTION:", ""))
		constantPrefixes := []string{
			"N*LOG(2PI)",
			"NIND*NETA*LOG(2PI)",
			"PRIOR",
		}
		for _, cp := range constantPrefixes {
			constantString = strings.TrimSpace(strings.ReplaceAll(constantString, cp, ""))
		}
		ofvDetails.ConstantToOFV = strToFloat(constantString)
	} else if strings.Contains(line, "OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT") {
		ofvDetails.OFVNoConstant = strToFloat(
			strings.TrimSpace(strings.ReplaceAll(line, "OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:", "")))
	} else if strings.Contains(line, "OBJECTIVE FUNCTION VALUE WITH CONSTANT") {
		ofvDetails.OFVWithConstant = strToFloat(
			strings.TrimSpace(strings.ReplaceAll(line, "OBJECTIVE FUNCTION VALUE WITH CONSTANT:", "")))
	}

	return allOfvDetails
}

func parseConditionNumberLst(lines []string, start int, allCondDetails []ConditionNumDetails) []ConditionNumDetails {
	// always modify the most recently created ConditionNumDetails
	condDetails := &allCondDetails[len(allCondDetails)-1]

	condDetails.ConditionNumber = mustCalculateConditionNumber(lines, start)

	return allCondDetails
}

func mustCalculateConditionNumber(lines []string, start int) float64 {
	// go until line of ints
	for i, line := range lines[start:] {
		sub := strings.TrimSpace(line)
		if len(sub) > 0 {
			vals := strings.Fields(sub)
			if len(vals) > 1 {
				one, err := strconv.Atoi(vals[0])
				if err == nil && one == 1 {
					two, err := strconv.Atoi(vals[1])
					if err == nil && two == 2 {
						start = start + i

						break
					}
				}
			}
		}
	}

	// go until blank line
	for i, line := range lines[start:] {
		sub := strings.TrimSpace(line)
		if len(sub) == 0 {
			start = start + i + 1

			break
		}
	}

	// go until non-blank line and build eigenvalues vector
	var eigenvalues []float64
	eigenstarted := false
	for _, line := range lines[start:] {
		sub := strings.TrimSpace(line)
		if len(sub) == 0 {
			if eigenstarted {
				break
			} else {
				continue
			}
		}

		eigenstarted = true
		for _, s := range strings.Fields(line) {
			eigenvalue, err := parseFloatReplaceNaN(s)
			if err != nil {
				panic(fmt.Sprintf("Attempting to calculate condition number but could not parse eigenvalues -- %v", err))
			}
			eigenvalues = append(eigenvalues, eigenvalue)
		}
	}

	sort.Float64s(eigenvalues)
	ratio := -1.0 // if eigenvalues contain a zero we return condition number of -1
	if eigenvalues[0] != 0 {
		ratio = eigenvalues[len(eigenvalues)-1] / eigenvalues[0]
	}

	return ratio
}

func getLargeConditionNumber(allCondDetails []ConditionNumDetails) bool {
	// TODO: get largeNumberLimit from config
	// or derive. something like (number of parameters) * 10
	largeNumberLimit := 1000.0
	cb := make([]bool, len(allCondDetails))
	for i, cn := range allCondDetails {
		cb[i] = cn.ConditionNumber > largeNumberLimit
	}

	return utils.AnyTrue(cb)
}
