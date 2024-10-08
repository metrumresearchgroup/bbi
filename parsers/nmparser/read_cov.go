package parser

import (
	"strings"
)

func parseCovLines(lines []string) ExtData {
	var estimationMethods []string
	var estimationSteps [][]string
	var estimationStep []string
	var paramNames []string
	for _, line := range lines {
		if strings.HasPrefix(line, "TABLE") {
			estimationMethods = append(estimationMethods, line)
			if len(estimationStep) > 0 {
				// if not the first one, means its ready to save off
				estimationSteps = append(estimationSteps, estimationStep)
				estimationStep = []string{} //reset
			}
		} else {
			if strings.HasPrefix(line, " NAME") {
				if len(paramNames) == 0 {
					paramNames = strings.Fields(line)
				}

				continue
			}

			if strings.HasPrefix(line, " THETA") {
				estimationStep = append(estimationStep, strings.TrimSpace(line))
			}
		}
	}
	estimationSteps = append(estimationSteps, estimationStep)

	return ExtData{
		EstimationMethods: estimationMethods,
		ParameterNames:    paramNames,
		EstimationLines:   estimationSteps,
	}
}

func getThetaMatrix(lines []string) [][]float64 {
	var matrix [][]float64
	len := len(lines)

	for _, line := range lines {
		if strings.HasPrefix(line, "THETA") {
			fields := strings.Fields(line)
			thetas := make([]float64, len)

			for n := 1; n <= len; n++ {
				if f, err := parseFloatReplaceNaN(fields[n]); err == nil {
					thetas[n-1] = f
				} else {
					panic("error converting value in cov file to number: " + fields[n])
				}
			}
			matrix = append(matrix, thetas)
		}
	}
	// no transpose required to create column-major matrix as this matrix is symmetrical
	return matrix
}

// GetThetaValues extracts the theta values from a file that has
// thetas, such as cov/cor/ext data and returns the
// data as a FlatArray.
func GetThetaValues(lines []string) []FlatArray {
	var result []FlatArray
	data := parseCovLines(lines)

	for _, d := range data.EstimationLines {
		m := getThetaMatrix(d)
		thetas := MakeFlatArray(m, len(m))
		result = append(result, thetas)
	}

	return result
}
