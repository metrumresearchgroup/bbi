package parser

import (
	"log"
	"strconv"
	"strings"
)

// ParseShkLines ...
func ParseShkLines(lines []string) ExtData {
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
			if strings.HasPrefix(line, "TYPE") {
				if len(paramNames) == 0 {
					paramNames = strings.Fields(line)
				}
				continue
			}
			estimationStep = append(estimationStep, strings.TrimSpace(line))
		}
	}
	estimationSteps = append(estimationSteps, estimationStep)
	return ExtData{
		EstimationMethods: estimationMethods,
		ParameterNames:    paramNames,
		EstimationLines:   estimationSteps,
	}
}

// ParseShkData ...
func ParseShkData(extData ExtData, etaCount, epsCount int) []ShrinkageDetails {
	var shrinkageDetails []ShrinkageDetails
	for _, lines := range extData.EstimationLines {
		shk := ParseShrinkage(lines, etaCount, epsCount)
		shrinkageDetails = append(shrinkageDetails, shk)
	}
	return shrinkageDetails
}

// ParseShrinkage ...
func ParseShrinkage(lines []string, etaCount, epsCount int) ShrinkageDetails {
	var shrinkageDetails ShrinkageDetails
	for _, line := range lines {
		fields := strings.Fields(line)
		length := len(fields)
		if length > 0 {
			switch fields[0] {
			case "1":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails.EtaBar = append(shrinkageDetails.EtaBar, strToFloat(fields[n]))
					}
				}
			case "2":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails.EtaBarSE = append(shrinkageDetails.EtaBarSE, strToFloat(fields[n]))
					}
				}
			case "3":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails.Pval = append(shrinkageDetails.Pval, strToFloat(fields[n]))
					}
				}
			case "4":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails.EtaSD = append(shrinkageDetails.EtaSD, strToFloat(fields[n]))
					}
				}
			case "5":
				for n := 2; n < (length); n++ {
					if n-2 < epsCount {
						shrinkageDetails.EpsSD = append(shrinkageDetails.EpsSD, strToFloat(fields[n]))
					}
				}
			case "6":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails.EbvSD = append(shrinkageDetails.EbvSD, strToFloat(fields[n]))
					}
				}
			case "7":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails.NumSubjects = append(shrinkageDetails.NumSubjects, strToFloat(fields[n]))
					}
				}
			case "8":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails.EtaVR = append(shrinkageDetails.EtaVR, strToFloat(fields[n]))
					}
				}
			// the NM doc seems to be incorrect for these values, 9 and 10 are swapped
			case "10":
				for n := 2; n < (length); n++ {
					if n-2 < epsCount {
						shrinkageDetails.EpsVR = append(shrinkageDetails.EpsVR, strToFloat(fields[n]))
					}
				}
			case "9":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails.EbvVR = append(shrinkageDetails.EbvVR, strToFloat(fields[n]))
					}
				}
			default:
				log.Printf("ParseShrinkage, unknown field type: %s", fields[0])
			}
		}
	}
	return shrinkageDetails
}

func strToFloat(s string) float64 {
	var f float64
	f, err := strconv.ParseFloat(s, 64)
	if err != nil {
		f = 0
	}
	return f
}
