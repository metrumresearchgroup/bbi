package parser

import (
	"strings"
)

// ParameterEstimates contains the final parameter estimate values
type ParameterEstimates struct {
	Theta []float64
	Omega []float64
	Sigma []float64
}

// ParseFinalParameterEstimatesFromLst parses the final estimates of model parameters from lst file
func ParseFinalParameterEstimatesFromLst(lines []string) ParameterEstimates {
	var thetaStart int
	var omegaStart int
	var sigmaStart int
	for i, line := range lines {
		switch {
		case strings.Contains(line, "THETA - VECTOR OF FIXED EFFECTS PARAMETERS"):
			thetaStart = i
		case strings.Contains(line, "OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS"):
			omegaStart = i
		case strings.Contains(line, "SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS"):
			sigmaStart = i
		default:
			continue
		}
	}
	thetaParsed := ParseThetaResults(lines[thetaStart:omegaStart])
	omegaParsed := ParseBlockResults(lines[omegaStart:sigmaStart])
	sigmaParsed := ParseBlockResults(lines[sigmaStart:])

	return ParameterEstimates{thetaParsed, omegaParsed, sigmaParsed}
}
