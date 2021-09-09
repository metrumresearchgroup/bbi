package parser

import (
	"strings"
)

// ParseFinalParameterEstimatesFromLst parses the final estimates of model parameters from lst file.
func ParseFinalParameterEstimatesFromLst(lines []string) ParametersResult {
	var thetaStart int
	var omegaStart int
	var sigmaStart int
	var sigmaEnd int
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

	for n := sigmaStart; n < len(lines); n++ {
		if strings.HasPrefix(lines[n], "1") {
			sigmaEnd = n

			break
		}
	}

	var sigmaParsed []float64
	thetaParsed := ParseThetaResults(lines[thetaStart:omegaStart])
	omegaEnd := sigmaStart
	// if sigmaStart is 0 then there are no sigmas, which can happen
	// for certain types of models
	if omegaEnd == 0 {
		omegaEnd = len(lines) - 1
	}

	omegaParsed := ParseBlockResults(lines[omegaStart:omegaEnd])
	if sigmaStart == 0 {
		// just use default
	} else if sigmaEnd > sigmaStart {
		sigmaParsed = ParseBlockResults(lines[sigmaStart:sigmaEnd])
	} else {
		sigmaParsed = ParseBlockResults(lines[sigmaStart:])
	}

	return ParametersResult{thetaParsed, omegaParsed, sigmaParsed}
}
