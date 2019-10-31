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
func ParseShkData(extData ExtData, etaCount, epsCount int) [][]ShrinkageDetails {
	var shrinkageDetails [][]ShrinkageDetails
	for _, lines := range extData.EstimationLines {
		shk := ParseShrinkage(lines, etaCount, epsCount)
		shrinkageDetails = append(shrinkageDetails, shk)
	}
	return shrinkageDetails
}

// ParseShrinkage ...
func ParseShrinkage(lines []string, etaCount, epsCount int) []ShrinkageDetails {
	var shrinkageDetails []ShrinkageDetails
	for _, line := range lines {
		fields := strings.Fields(line)
		length := len(fields)
		if length > 0 {

			subpop, err := strconv.Atoi(fields[1])
			if err != nil{
				panic("Error parsing shrinkage")
			}
			
			if len(shrinkageDetails) < subpop {
				shrinkageDetails = append(shrinkageDetails, ShrinkageDetails{})
			}
			i := subpop-1
			shrinkageDetails[i].SubPop = int64(subpop)

			switch fields[0] {
			case "1":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails[i].EtaBar = append(shrinkageDetails[i].EtaBar, strToFloat(fields[n]))
					}
				}
			case "2":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails[i].EtaBarSE = append(shrinkageDetails[i].EtaBarSE, strToFloat(fields[n]))
					}
				}
			case "3":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails[i].Pval = append(shrinkageDetails[i].Pval, strToFloat(fields[n]))
					}
				}
			case "4":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails[i].EtaSD = append(shrinkageDetails[i].EtaSD, strToFloat(fields[n]))
					}
				}
			case "5":
				for n := 2; n < (length); n++ {
					if n-2 < epsCount {
						shrinkageDetails[i].EpsSD = append(shrinkageDetails[i].EpsSD, strToFloat(fields[n]))
					}
				}
			case "6":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails[i].EbvSD = append(shrinkageDetails[i].EbvSD, strToFloat(fields[n]))
					}
				}
			case "7":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails[i].NumSubjects = append(shrinkageDetails[i].NumSubjects, strToFloat(fields[n]))
					}
				}
			case "8":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails[i].EtaVR = append(shrinkageDetails[i].EtaVR, strToFloat(fields[n]))
					}
				}
			// the NM doc seems to be incorrect for these values, 9 and 10 are swapped
			case "10":
				for n := 2; n < (length); n++ {
					if n-2 < epsCount {
						shrinkageDetails[i].EpsVR = append(shrinkageDetails[i].EpsVR, strToFloat(fields[n]))
					}
				}
			case "9":
				for n := 2; n < (length); n++ {
					if n-2 < etaCount {
						shrinkageDetails[i].EbvVR = append(shrinkageDetails[i].EbvVR, strToFloat(fields[n]))
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
		f = DefaultFloat64
	}
	return f
}
