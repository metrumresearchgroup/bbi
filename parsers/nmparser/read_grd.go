package parser

import (
	"strconv"
	"strings"

	"github.com/metrumresearchgroup/babylon/utils"
)

// ParseGrdLines parses out the ext lines into a data structure for final parameter processing.
func ParseGrdLines(lines []string) ExtData {
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
			if strings.HasPrefix(line, " ITER") {
				if len(paramNames) == 0 {
					paramNames = strings.Fields(line)
				}
				continue
			}
			estimationStep = append(estimationStep, strings.TrimSpace(line))
		}
	}
	estimationSteps = append(estimationSteps, estimationStep)
	// final estimation step will still need to get added to estimationSteps
	return ExtData{
		EstimationMethods: estimationMethods,
		ParameterNames:    paramNames,
		EstimationLines:   estimationSteps,
	}
}

//ParseGrdData returns the ExtData in the structure of final parameter estimates
func ParseGrdData(ed ExtData) ([]ParametersData, ParameterNames) {
	var allParametersData []ParametersData
	var thetas []string
	for _, name := range ed.ParameterNames {
		switch {
		case strings.HasPrefix(name, "GRD"):
			thetas = append(thetas, name)
		default:
			continue
		}
	}
	for estIndex, method := range ed.EstimationMethods {
		parametersData := ParametersData{
			Method: method,
		}
		for _, line := range ed.EstimationLines[estIndex] {
			fields := strings.Fields(line)
			if len(fields) > 0 {
				result := make([]float64, len(fields)-1)
				for i, val := range fields {
					// slip iteration value
					if i == 0 {
						continue
					}
					if n, err := strconv.ParseFloat(val, 64); err == nil {
						result[i-1] = n
					} else {
						panic("error converting value in grd file to number: " + val)
					}
				}
				parametersData.Fixed = ParametersResult{
					Theta: result,
				}
			}
		}
		allParametersData = append(allParametersData, parametersData)
	}
	return allParametersData, ParameterNames{
		Theta: thetas,
	}
}

// HasZeroGradient returns Status.True if any float in the slice is zero
func HasZeroGradient(floats []float64) Status {
	if utils.HasZero(floats) == true {
		return True
	}
	return False
}
