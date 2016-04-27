package parser

import (
	"strconv"
	"strings"
)

// FinalParameterEstimates contains the final parameter estimate values
type ParameterStructure struct {
	Theta int
	Omega []string
	Sigma []string
}

// ParseFinalParameterEstimates parses the final estimates of model parameters from lst file
func ParseParameterStructure(lines []string) ParameterStructure {
	var thetaLength int
	var omegaFormStart int
	var omegaFormEnd int
	var sigmaFormStart int
	var sigmaFormEnd int
	for i, line := range lines {
		switch {
		case strings.Contains(line, "0LENGTH OF THETA:"):
			thetaLength, _ = strconv.Atoi(strings.TrimSpace(strings.SplitAfter(line, ":")[1]))
		case strings.Contains(line, "0OMEGA HAS "):
			omegaFormStart = i
		case strings.Contains(line, "0DEFAULT OMEGA BOUNDARY"):
			omegaFormEnd = i
		case strings.Contains(line, "0SIGMA HAS "):
			sigmaFormStart = i
		case strings.Contains(line, "0DEFAULT SIGMA BOUNDARY"):
			sigmaFormEnd = i
		default:
			continue
		}
	}
	omegaStructure := ParseOmegaStructure(lines[omegaFormStart:omegaFormEnd])
	sigmaStructure := ParseSigmaStructure(lines[sigmaFormStart:sigmaFormEnd])
	return ParameterStructure{thetaLength, omegaStructure, sigmaStructure}
}
