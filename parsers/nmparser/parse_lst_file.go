package parser

import (
	"math"
	"sort"
	"strconv"
	"strings"
)

func parseShrinkage(line string, shrinkageDetails ShrinkageDetails) ShrinkageDetails {
	if strings.Contains(line, "ETASHRINKSD(%)") {
		shrinkageDetails.EtaSD = parseFloats(line, "ETASHRINKSD(%)")
	} else if strings.Contains(line, "ETASHRINKVR(%)") {
		shrinkageDetails.EtaVR = parseFloats(line, "ETASHRINKVR(%)")
	} else if strings.Contains(line, "EBVSHRINKSD(%)") {
		shrinkageDetails.EbvSD = parseFloats(line, "EBVSHRINKSD(%)")
	} else if strings.Contains(line, "EBVSHRINKVR(%)") {
		shrinkageDetails.EbvVR = parseFloats(line, "EBVSHRINKVR(%)")
	} else if strings.Contains(line, "EPSSHRINKSD(%)") {
		shrinkageDetails.EpsSD = parseFloats(line, "EPSSHRINKSD(%)")
	} else if strings.Contains(line, "EPSSHRINKVR(%)") {
		shrinkageDetails.EpsVR = parseFloats(line, "EPSSHRINKVR(%)")
	} else if strings.Contains(line, "ETAshrink(%)") {
		line = strings.Replace(line, ":", "", 1)
		shrinkageDetails.EtaSD = parseFloats(line, "ETAshrink(%)")
	} else if strings.Contains(line, "EBVshrink(%)") {
		line = strings.Replace(line, ":", "", 1)
		shrinkageDetails.EbvSD = parseFloats(line, "EBVshrink(%)")
	} else if strings.Contains(line, "EPSshrink(%)") {
		line = strings.Replace(line, ":", "", 1)
		shrinkageDetails.EpsVR = parseFloats(line, "EPSshrink(%)")
	}

	return shrinkageDetails
}

func parseFloats(line, name string) []float64 {
	var floats []float64
	values := strings.Fields(strings.TrimSpace(strings.Replace(line, name, "", -1)))
	for _, value := range values {
		fvalue, err := strconv.ParseFloat(value, 64)
		if err != nil {
			fvalue = DefaultFloat64
		}
		floats = append(floats, fvalue)
	}
	return floats
}

func parseOFV(line string, ofvDetails OfvDetails) OfvDetails {
	if strings.Contains(line, "#OBJV:") {
		result := strings.Replace(line, "*", "", -1)
		ofvDetails.OFVNoConstant, _ = strconv.ParseFloat(
			strings.TrimSpace(strings.Replace(result, "#OBJV:", "", -1)),
			64)
	} else if strings.Contains(line, "CONSTANT TO OBJECTIVE FUNCTION") {
		ofvDetails.OFV, _ = strconv.ParseFloat(
			strings.TrimSpace(strings.Replace(line, "N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:", "", -1)),
			64)
	} else if strings.Contains(line, "OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT") {
		ofvDetails.OFVNoConstant, _ = strconv.ParseFloat(
			strings.TrimSpace(strings.Replace(line, "OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:", "", -1)),
			64)
	} else if strings.Contains(line, "OBJECTIVE FUNCTION VALUE WITH CONSTANT") {
		ofvDetails.OFVWithConstant, _ = strconv.ParseFloat(
			strings.TrimSpace(strings.Replace(line, "OBJECTIVE FUNCTION VALUE WITH CONSTANT:", "", -1)),
			64)
	}
	return ofvDetails
}

func getLargeConditionNumberStatus(lines []string, start int, largeNumberLimit float64) HeuristicStatus {

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

	var eigenvalues []float64
	for _, s := range strings.Fields(lines[start]) {
		eigenvalue, err := strconv.ParseFloat(s, 64)
		if err == nil {
			eigenvalues = append(eigenvalues, eigenvalue)
		}
	}

	if len(eigenvalues) >= 2 {
		sort.Float64s(eigenvalues)
		ratio := eigenvalues[len(eigenvalues)-1] / eigenvalues[0]
		if ratio > largeNumberLimit {
			return HeuristicTrue
		}
	}

	return HeuristicFalse
}

func getMatrixData(lines []string, start int) MatrixData {

	var matrix [][]float64
	var thetaCount int
	var omegaCount int
	var sigmaCount int

	// go until blank line
	for i, line := range lines[start:] {
		sub := strings.TrimSpace(line)
		if len(sub) == 0 {
			start = start + i
			break
		}
	}

	// go until not blank line
	for i, line := range lines[start:] {
		sub := strings.TrimSpace(line)
		if len(sub) > 0 {
			start = start + i
			break
		}
	}

	// go until not a blank line, should be names of columns: TH 1      TH 2      TH 3 ....
	// and get lines until the next blank line is found (columns names can span more than one line)
	var columns []string
	for i, line := range lines[start:] {
		sub := strings.TrimSpace(line)
		if len(sub) > 0 {
			c := strings.Split(sub, "    ")
			for _, col := range c {
				columns = append(columns, strings.TrimSpace(col))
			}
			start = start + i
		} else {
			start = start + 1
			break
		}
	}

	for _, column := range columns {
		if strings.HasPrefix(column, "TH") {
			thetaCount++
		} else if strings.HasPrefix(column, "OM") {
			omegaCount++
		} else if strings.HasPrefix(column, "SG") {
			sigmaCount++
		}
	}

	dim := len(columns)
	if dim > 0 {
		matrix = make([][]float64, dim)
		for i := range matrix {
			matrix[i] = make([]float64, dim)
		}
		// get lines that start with + and collect until blank line
		// (row values can span more than one line)
		var rows []string
		for _, line := range lines[start:] {

			// skip blank line
			clean := strings.TrimSpace(line)
			if clean == "" {
				continue
			}

			// skip the name of the parameter
			if contains(columns, clean) {
				continue
			}

			// this line is either the start of values (starts with +) or a continuation of values (starts with ..... or a value)
			if strings.HasPrefix(clean, "+") {
				clean = strings.TrimSpace(strings.Replace(strings.Replace(line, "+", "", -1), ",", "", -1))
				rows = append(rows, clean)
			} else {
				// if there is not a +, append the values to the previous row
				if len(rows) > 0 {
					// section ends with a line with "1"
					if clean == "1" {
						break
					}
					previousrow := len(rows) - 1
					clean = strings.TrimSpace(strings.Replace(strings.Replace(line, "+", "", -1), ",", "", -1))
					rows[previousrow] = rows[previousrow] + " " + clean
				}
			}
		}

		// populate matrix
		for j, row := range rows {
			for k, cell := range strings.Fields(row) {
				value, err := strconv.ParseFloat(cell, 64)
				if err == nil {
					matrix[j][k] = value
				} else {
					matrix[j][k] = 0
				}
			}
		}
		// a symmetric matrix is equal to its transpose, however in the case of the lst file
		// the upper diagonal elements are omitted, so a transpose is required to deliver
		// a column-major matrix
		matrix = transpose(matrix)
	}

	return MatrixData{
		Values:     matrix,
		ThetaCount: thetaCount,
		OmageCount: omegaCount,
		SigmaCount: sigmaCount,
	}
}

func getCorrelationStatus(lines []string, start int, correlationLimit float64) HeuristicStatus {

	matrix := getMatrixData(lines, start).Values

	if len(matrix) > 0 {
		// check for large numbers in the off-diagonal values of the correlation matrix
		for j := range matrix {
			for k, cell := range matrix[j] {
				if k == j || cell == 0 {
					continue
				}
				if math.Abs(cell) >= correlationLimit {
					return HeuristicFalse
				}
			}
		}
	} else {
		return HeuristicUndefined
	}

	return HeuristicTrue
}

func getThetaValues(lines []string, start int) FlatArray {
	matrixData := getMatrixData(lines, start)
	thetas := MakeFlatArray(matrixData.Values, matrixData.ThetaCount)
	return thetas
}

func transpose(slice [][]float64) [][]float64 {
	xl := len(slice[0])
	yl := len(slice)
	result := make([][]float64, xl)
	for i := range result {
		result[i] = make([]float64, yl)
	}
	for i := 0; i < xl; i++ {
		for j := 0; j < yl; j++ {
			result[i][j] = slice[j][i]
		}
	}
	return result
}

func contains(a []string, value string) bool {
	for _, s := range a {
		if strings.TrimSpace(s) == strings.TrimSpace(value) {
			return true
		}
	}
	return false
}

func parseGradient(lines []string) (hasFinalZero HeuristicStatus) {

	if len(lines) == 0 {
		return HeuristicUndefined
	}

	fields := strings.Fields(strings.TrimSpace(strings.Replace(lines[len(lines)-1], "GRADIENT:", "", -1)))
	if len(fields) > 0 {
		result := make([]float64, len(fields))
		for i, val := range fields {
			n, err := strconv.ParseFloat(val, 64)
			if err != nil {
				n = DefaultFloat64
			}
			result[i] = n
		}
		return HasZeroGradient(result)
	}
	return HeuristicFalse
}

// get gradient lines until a blank line is reached
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

// ParseLstEstimationFile parses the lst file
func ParseLstEstimationFile(lines []string) ModelOutput {
	ofvDetails := NewOfvDetails()
	shrinkageDetails := make([]ShrinkageDetails, 1)
	runHeuristics := NewRunHeuristics()
	var finalParameterEstimatesIndex int
	var standardErrorEstimateIndex int
	var covarianceMatrixEstimateIndex int
	var correlationMatrixEstimateIndex int
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
		case strings.Contains(line, "#OBJV"):
			ofvDetails = parseOFV(line, ofvDetails)
		case strings.Contains(line, "CONSTANT TO OBJECTIVE FUNCTION"):
			ofvDetails = parseOFV(line, ofvDetails)
		case strings.Contains(line, "OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT"):
			ofvDetails = parseOFV(line, ofvDetails)
		case strings.Contains(line, "OBJECTIVE FUNCTION VALUE WITH CONSTANT"):
			ofvDetails = parseOFV(line, ofvDetails)
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
		case strings.Contains(line, "ETASHRINK"):
			shrinkageDetails[0] = parseShrinkage(line, shrinkageDetails[0])
		case strings.Contains(line, "EBVSHRINK"):
			shrinkageDetails[0] = parseShrinkage(line, shrinkageDetails[0])
		case strings.Contains(line, "EPSSHRINK"):
			shrinkageDetails[0] = parseShrinkage(line, shrinkageDetails[0])
		case strings.Contains(line, "ETAshrink"):
			shrinkageDetails[0] = parseShrinkage(line, shrinkageDetails[0])
		case strings.Contains(line, "EBVshrink"):
			shrinkageDetails[0] = parseShrinkage(line, shrinkageDetails[0])
		case strings.Contains(line, "EPSshrink"):
			shrinkageDetails[0] = parseShrinkage(line, shrinkageDetails[0])
		case strings.Contains(line, "0MINIMIZATION SUCCESSFUL"):
			runHeuristics.MinimizationSuccessful = HeuristicTrue
		case strings.Contains(line, "GRADIENT:"):
			gradientLines = append(gradientLines, getGradientLine(lines, i))
		case strings.Contains(line, "RESET HESSIAN"):
			runHeuristics.HessianReset = HeuristicTrue
		case strings.Contains(line, "PARAMETER ESTIMATE IS NEAR ITS BOUNDARY"):
			runHeuristics.ParameterNearBoundary = HeuristicTrue
		case strings.Contains(line, "COVARIANCE STEP OMITTED: NO"):
			runHeuristics.CovarianceStepOmitted = HeuristicTrue
		case strings.Contains(line, "EIGENVALUES OF COR MATRIX OF ESTIMATE"):
			// TODO: get largeNumberLimit from config
			// or derive. something like (number of parameters) * 10
			runHeuristics.LargeConditionNumber = getLargeConditionNumberStatus(lines, i, 1000.0)
		case strings.Contains(line, "CORRELATION MATRIX OF ESTIMATE"):
			correlationMatrixEstimateIndex = i
			// TODO: get correlationLimit from config
			runHeuristics.CorrelationsOk = getCorrelationStatus(lines, i, 0.90)

		default:
			continue
		}
	}

	runHeuristics.HasFinalZeroGradient = parseGradient(gradientLines)

	var finalParameterEst ParametersResult
	var finalParameterStdErr ParametersResult
	//var parameterNames ParameterNames
	var covTheta, corTheta FlatArray

	if standardErrorEstimateIndex > finalParameterEstimatesIndex {
		finalParameterEst = ParseFinalParameterEstimatesFromLst(lines[finalParameterEstimatesIndex:standardErrorEstimateIndex])
	}

	if covarianceMatrixEstimateIndex > 0 {
		covTheta = getThetaValues(lines, covarianceMatrixEstimateIndex)
	}

	if correlationMatrixEstimateIndex > 0 {
		corTheta = getThetaValues(lines, correlationMatrixEstimateIndex)
	}

	if covarianceMatrixEstimateIndex > standardErrorEstimateIndex {
		finalParameterStdErr = ParseFinalParameterEstimatesFromLst(lines[standardErrorEstimateIndex:covarianceMatrixEstimateIndex])
	}

	// TODO: replace parsing parameter names
	// if endSigmaIndex > startThetaIndex {
	// 	parameterNames = ParseParameterNames(lines[startThetaIndex:endSigmaIndex])
	// }
	// TODO re-replace parameter data from lst
	result := ModelOutput{
		RunHeuristics: runHeuristics,
		RunDetails:    ParseRunDetails(lines),
		ParametersData: []ParametersData{
			ParametersData{
				Estimates: finalParameterEst,
				StdErr:    finalParameterStdErr,
			},
		},
		ParameterNames: NewDefaultParameterNames(len(finalParameterEst.Theta), len(finalParameterEst.Omega), len(finalParameterEst.Sigma)),

		OFV:              ofvDetails,
		ShrinkageDetails: [][]ShrinkageDetails{shrinkageDetails},
		CovarianceTheta:  []FlatArray{covTheta},
		CorrelationTheta: []FlatArray{corTheta},
	}
	return result
}
