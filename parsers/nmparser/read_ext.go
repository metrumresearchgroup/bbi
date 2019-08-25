package parser

import (
	"strings"
)

// ParseExtLines parses out the ext lines into a data structure for final parameter processing.
func ParseExtLines(lines []string) ExtData {
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
			if strings.HasPrefix(line, " ITER") && len(paramNames) == 0 {
				paramNames = strings.Fields(line)
			} else {
				continue
			}
			estimationStep = append(estimationStep, line)
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

// GetFinalParameterEstimates returns the ExtData in the structure of final parameter estimates
// func GetFinalParameterEstimates(extData ExtData) {
// 	var finalParameterEstimates ParametersData
// 	var finalParameterStdErr ParametersData
// }
