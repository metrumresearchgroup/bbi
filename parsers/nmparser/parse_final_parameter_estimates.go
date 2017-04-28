package parser

import (
	"strings"
)

// FinalParameterEstimates contains the final parameter estimate values
type FinalParameterEstimates struct {
	Theta     []float64
	Omega     []float64
	Sigma     []float64
	OmegaCorr []float64
	SigmaCorr []float64
}

// ParseFinalParameterEstimates parses the final estimates of model parameters from lst file
func ParseFinalParameterEstimates(lines []string) FinalParameterEstimates {
	var thetaStart int
	var omegaStart int
	var sigmaStart int
	var omegaCorrStart int
	var sigmaCorrStart int
	for i, line := range lines {
		switch {
		case strings.Contains(line, "THETA - VECTOR OF FIXED EFFECTS PARAMETERS"):
			thetaStart = i
		case strings.Contains(line, "OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS"):
			omegaStart = i
		case strings.Contains(line, "SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS"):
			sigmaStart = i
		case strings.Contains(line, "OMEGA - CORR MATRIX"):
			omegaCorrStart = i
		case strings.Contains(line, "SIGMA - CORR MATRIX"):
			sigmaCorrStart = i
		default:
			continue
		}
	}
	thetaParsed := ParseThetaResults(lines[thetaStart:omegaStart])
	omegaParsed := ParseBlockResults(lines[omegaStart:sigmaStart])
	sigmaParsed := ParseBlockResults(lines[sigmaStart:omegaCorrStart])
	omegaCorrParsed := ParseBlockResults(lines[omegaCorrStart:sigmaCorrStart])
	sigmaCorrParsed := ParseBlockResults(lines[sigmaCorrStart:])

	return FinalParameterEstimates{thetaParsed, omegaParsed, sigmaParsed, omegaCorrParsed, sigmaCorrParsed}
}
