package parser

import (
	"sort"
	"strconv"
	"strings"
)

func parseShrinkage(line string, shrinkageDetails ShrinkageDetails) ShrinkageDetails {
	if strings.Contains(line, "ETASHRINKSD(%)") {
		shrinkageDetails.Eta.SD = parseFloats(line, "ETASHRINKSD(%)")
	} else if strings.Contains(line, "ETASHRINKVR(%)") {
		shrinkageDetails.Eta.VR = parseFloats(line, "ETASHRINKVR(%)")
	} else if strings.Contains(line, "EBVSHRINKSD(%)") {
		shrinkageDetails.Ebv.SD = parseFloats(line, "EBVSHRINKSD(%)")
	} else if strings.Contains(line, "EBVSHRINKVR(%)") {
		shrinkageDetails.Ebv.VR = parseFloats(line, "EBVSHRINKVR(%)")
	} else if strings.Contains(line, "EPSSHRINKSD(%)") {
		shrinkageDetails.Eps.SD = parseFloats(line, "EPSSHRINKSD(%)")
	} else if strings.Contains(line, "EPSSHRINKVR(%)") {
		shrinkageDetails.Eps.VR = parseFloats(line, "EPSSHRINKVR(%)")
	}
	return shrinkageDetails
}

func parseFloats(line, name string) []float64 {
	var floats []float64
	values := strings.Fields(strings.TrimSpace(strings.Replace(line, name, "", -1)))
	for _, value := range values {
		fvalue, _ := strconv.ParseFloat(value, 64)
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

func setLargeConditionNumber(lines []string, start int, largeNumberLimit float64) HeuristicStatus {

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

func setCorrelationsOk(lines []string, start int, correlationLimit float64) HeuristicStatus {

	// var matrix [][]float64

	// // go until blank line
	// for i, line := range lines[start:] {
	// 	sub := strings.TrimSpace(line)
	// 	if len(sub) == 0 {
	// 		start = start + i
	// 		break
	// 	}
	// }

	// // go until not blank line
	// for i, line := range lines[start:] {
	// 	sub := strings.TrimSpace(line)
	// 	if len(sub) > 0 {
	// 		start = start + i
	// 		break
	// 	}
	// }

	// // go until not blank line, should be names of columns: TH 1      TH 2      TH 3 ....
	// // and get lines until next blank line
	// var columns []string
	// for i, line := range lines[start:] {
	// 	sub := strings.TrimSpace(line)
	// 	if len(sub) > 0 {
	// 		c := strings.Split(sub, "    ")
	// 		for _, col := range c {
	// 			columns = append(columns, strings.TrimSpace(col))
	// 		}
	// 		//columns = append(columns, strings.Split(sub, "    ")...)
	// 		start = start + i
	// 	} else {
	// 		start = start + i
	// 		break
	// 	}
	// }

	// dim := len(columns)
	// if dim > 0 {
	// 	matrix = make([][]float64, dim)
	// 	for i := range matrix {
	// 		matrix[i] = make([]float64, dim)
	// 	}
	// 	// get lines that start with + and collect until blank line
	// 	var rows []string
	// 	for _, line := range lines[start:] {

	// 		clean := strings.TrimSpace(line)
	// 		if clean == "" {
	// 			continue
	// 		}

	// 		if contains(columns, clean) {
	// 			continue
	// 		}

	// 		clean = strings.TrimSpace(strings.Replace(strings.Replace(line, "+", "", -1), ",", "", -1))

	// 		// if clean does not contain +, append to last row
	// 		rows = append(rows, strings.Split(clean, "    ")...)

	// 		// if strings.HasPrefix(line, "+") {
	// 		// 	clean := strings.TrimSpace(strings.Replace(strings.Replace(line, "+", "", -1), ",", "", -1))
	// 		// 	rows = append(rows, strings.Split(clean, "    ")...)
	// 		// } else {
	// 		// 	clean := strings.TrimSpace(line)
	// 		// 	if strings.TrimSpace(clean) == "" || contains(columns, clean) { // or col name
	// 		// 		break
	// 		// 	} else {
	// 		// 		rows = append(rows, strings.Split(clean, "    ")...)
	// 		// 	}
	// 		// }
	// 	}

	// 	// populate matrix
	// 	for j, row := range rows {

	// 		fmt.Println("row:" + row)

	// 		for k, cell := range strings.Fields(row) {
	// 			value, err := strconv.ParseFloat(cell, 64)
	// 			if err == nil {
	// 				matrix[j][k] = value
	// 			} else {
	// 				matrix[j][k] = 0
	// 			}
	// 		}
	// 	}

	// 	// check for large numbers (close to 1) in the off-diagonal values of the correlation matrix
	// 	for j := range matrix {
	// 		for k, cell := range matrix[j] {
	// 			if k == j || cell == 0 {
	// 				continue
	// 			}
	// 			if math.Abs(cell) >= correlationLimit {
	// 				return HeuristicFalse
	// 			}
	// 		}
	// 	}

	// }
	return HeuristicTrue
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
			n, _ := strconv.ParseFloat(val, 64)
			result[i] = n
		}
		return HasZeroGradient(result)
	}
	return HeuristicFalse
}

// ParseLstEstimationFile parses the lst file
func ParseLstEstimationFile(lines []string) ModelOutput {
	var ofvDetails OfvDetails
	var shrinkageDetails ShrinkageDetails
	var runHeuristics RunHeuristics
	var startParameterStructuresIndex int
	var endParameterStucturesIndex int
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
		case strings.Contains(line, "0LENGTH OF THETA"):
			startParameterStructuresIndex = i
		case strings.Contains(line, "0DEFAULT SIGMA BOUNDARY"):
			endParameterStucturesIndex = i
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
			shrinkageDetails = parseShrinkage(line, shrinkageDetails)
		case strings.Contains(line, "EBVSHRINK"):
			shrinkageDetails = parseShrinkage(line, shrinkageDetails)
		case strings.Contains(line, "EPSSHRINK"):
			shrinkageDetails = parseShrinkage(line, shrinkageDetails)
		case strings.Contains(line, "0MINIMIZATION SUCCESSFUL"):
			runHeuristics.MinimizationSuccessful = HeuristicTrue
		case strings.Contains(line, "GRADIENT:"):
			gradientLines = append(gradientLines, line)
		case strings.Contains(line, "RESET HESSIAN"):
			runHeuristics.HessianReset = HeuristicTrue
		case strings.Contains(line, "PARAMETER ESTIMATE IS NEAR ITS BOUNDARY"):
			runHeuristics.ParameterNearBoundary = HeuristicTrue
		case strings.Contains(line, "COVARIANCE STEP OMITTED: NO"):
			runHeuristics.CovarianceStepOmitted = HeuristicTrue
		case strings.Contains(line, "EIGENVALUES OF COR MATRIX OF ESTIMATE"):
			// TODO: get largeNumberLimit from config
			// or derive. something like (number of parameters) * 10
			runHeuristics.LargeConditionNumber = setLargeConditionNumber(lines, i, 1000.0)
		case strings.Contains(line, "CORRELATION MATRIX OF ESTIMATE"):
			runHeuristics.CorrelationsOk = setCorrelationsOk(lines, i, 0.95)

		default:
			continue
		}
	}

	runHeuristics.HasFinalZeroGradient = parseGradient(gradientLines)

	var finalParameterEst ParametersResult
	var finalParameterStdErr ParametersResult
	var parameterStructures ParameterStructures
	var parameterNames ParameterNames

	if standardErrorEstimateIndex > finalParameterEstimatesIndex {
		finalParameterEst = ParseFinalParameterEstimatesFromLst(lines[finalParameterEstimatesIndex:standardErrorEstimateIndex])
	}

	if covarianceMatrixEstimateIndex > standardErrorEstimateIndex {
		finalParameterStdErr = ParseFinalParameterEstimatesFromLst(lines[standardErrorEstimateIndex:covarianceMatrixEstimateIndex])
	}

	if (endParameterStucturesIndex) > startParameterStructuresIndex {
		parameterStructures = ParseParameterStructures(lines[startParameterStructuresIndex : endParameterStucturesIndex+1])
	}

	if endSigmaIndex > startThetaIndex {
		parameterNames = ParseParameterNames(lines[startThetaIndex:endSigmaIndex])
	}
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
		ParameterStructures: parameterStructures,
		ParameterNames:      parameterNames,
		OFV:                 ofvDetails,
		ShrinkageDetails:    shrinkageDetails,
	}
	return result
}
