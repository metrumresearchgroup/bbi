package parser

import (
	"log"
	"strconv"
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

// ParseParamsExt returns the ExtData in the structure of final parameter estimates
// the parameter names correspond to the names per the ext file (THETA1, THETA2, etc)
// per nonmem 7.4 the following information will be grabbed
// 1) Theburn-in iterations of the MCMCBayesian analysis are given negative values,starting at –NBURN, the number of burn-in iterations requested by the user. These are followed by positive iterations of the stationary phase.
// 2) The stochastic iterations of the SAEM analysis are given negative values. These are followed by positive iterations of the accumulation phase.
// 3) Iteration -1000000000 (negative one billion) indicates that this line contains the final result (thetas, omegas, and sigmas, and objective function) of the particular analysis. For BAYES analysis, this is the mean of the non-negative iterations (stationary samples) listed before it.
// 4) Iteration -1000000001 indicates that this line contains the standard errors of the final population parameters. For BAYES, it is the sample standard deviation of the stationary samples.
// 5) Iteration -1000000002 indicates that this line contains the eigenvalues of the correlation matrix of the variances of the final parameters.
// 6) Iteration -1000000003 indicates that this line contains the condition number , lowest, highest, Eigen values of the correlation matrix of the variances of the final parameters.
// 7) Iteration -1000000004 indicates this line contains the OMEGA and SIGMA elements in
// standard deviation/correlation format
// 8) Iteration-1000000005indicatesthislinecontainsthestandarderrorstotheOMEGAand
// SIGMA elements in standard deviation/correlation format
// 9) Iteration -1000000006 indicates 1 if parameter was fixed in estimation, 0 otherwise.
// 10) Iteration -1000000007 lists termination status (first item) followed by termination codes.
// See I.54 $EST: Additional Output Files Produced under root.xml (NM72) for interpreting the codes.
// nm741.doc 174 of 284
// NONMEM Users Guide: Introduction to NONMEM 7.4.1
// 11) Iteration -1000000008 lists the partial derivative of the likelihood (-1/2 OFV) with respect to each estimated parameter. This may be useful for using tests like the Lagrange multiplier test.
// 12) Additional special iteration number lines may be added in future versions of NONMEM.
func ParseParamsExt(ed ExtData) ([]ParametersData, ParameterNames) {
	var allParametersData []ParametersData
	// order in ext is theta/sigma/omega
	var thetas []string
	var sigmas []string
	var omegas []string
	for _, name := range ed.ParameterNames {
		switch {
		case strings.HasPrefix(name, "THETA"):
			thetas = append(thetas, name)
		case strings.HasPrefix(name, "OMEGA"):
			omegas = append(omegas, name)
		case strings.HasPrefix(name, "SIGMA"):
			sigmas = append(sigmas, name)
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
			result := make([]float64, len(fields)-2)
			var step int
			for i, val := range fields {
				// for the 0th element, this will be the nonmem flag
				// to declare what is on the line
				if i == 0 {
					step, _ = strconv.Atoi(val)

					continue
				}
				if i == len(fields)-1 {
					// this is the OBJ which we've gotten elsewhere
					continue
				}
				if n, err := parseFloatReplaceNaN(val); err == nil {
					result[i-1] = n
				} else {
					log.Println("Did you use a non-default format parameter? bbi does not support non-default format parameters.")
					panic("error converting value in ext file to number: " + val)
				}
			}

			switch {
			case step == -1000000000:
				parametersData.Estimates = ParametersResult{
					Theta: result[0:(len(thetas))],
					Omega: result[len(thetas)+len(sigmas):],
					Sigma: result[len(thetas):(len(thetas) + len(sigmas))],
				}
			case step == -1000000001:
				parametersData.StdErr = ParametersResult{
					Theta: result[:(len(thetas))],
					Omega: result[len(thetas)+len(sigmas):],
					Sigma: result[len(thetas):(len(thetas) + len(sigmas))],
				}
			case step == -1000000004:
				parametersData.RandomEffectSD = RandomEffectResult{
					Omega: result[len(thetas)+len(sigmas):],
					Sigma: result[len(thetas):(len(thetas) + len(sigmas))],
				}
			case step == -1000000005:
				parametersData.RandomEffectSDSE = RandomEffectResult{
					Omega: result[len(thetas)+len(sigmas):],
					Sigma: result[len(thetas):(len(thetas) + len(sigmas))],
				}
			case step == -1000000006:
				parametersData.Fixed = ParametersResult{
					Theta: result[0:(len(thetas))],
					Omega: result[len(thetas)+len(sigmas):],
					Sigma: result[len(thetas):(len(thetas) + len(sigmas))],
				}
			default:
				continue
			}
		}
		allParametersData = append(allParametersData, parametersData)
	}

	return allParametersData, ParameterNames{
		Theta: thetas,
		Omega: omegas,
		Sigma: sigmas,
	}
}

// ParseConditionNumberExt returns the condition number for each estimation method from ExtData
// per nonmem 7.4 the following information will be grabbed
// Iteration -1000000003 indicates that this line contains the condition number , lowest, highest, Eigenvalues of the correlation matrix of the variances of the final parameters.
// NONMEM Users Guide: Introduction to NONMEM 7.4.1.
func ParseConditionNumberExt(ed ExtData) []ConditionNumDetails {
	var allCondDetails []ConditionNumDetails

	for estIndex, method := range ed.EstimationMethods {
		method = strings.TrimSpace(strings.Split(method, ":")[1])
		condNum := DefaultFloat64
		for _, line := range ed.EstimationLines[estIndex] {
			fields := strings.Fields(line)
			step, _ := strconv.Atoi(fields[0])
			if step == -1000000003 {
				condNum = strToFloat(fields[1])

				break
			} else {
				continue
			}
		}
		allCondDetails = append(allCondDetails, NewConditionNumDetails(method, condNum))
	}

	return allCondDetails
}
