package parser

import (
	"strconv"
	"strings"
)

// ParameterStructures contains the final parameter estimate values such
// that for Omega and Sigma, the int slices represent which portion of
// the model definition corresponds to the parameter values. For example
// given mutiple $OMEGA statements, the values will correspond to different
// blocks,
// $OMEGA 0.04
// $OMEGA BLOCK(2)
// 0.04
// 0.01 0.04
//
// would correspond to a lower triangular structure
// 1
// 0 2
// 0 2 2
type ParameterStructures struct {
	Theta int
	Omega []int
	Sigma []int
}

func parseParameterStructure(lines []string) []int {
	if strings.Contains(lines[0], "SIMPLE") {
		omegaLength, _ := strconv.Atoi(strings.TrimSpace(strings.SplitAfter(lines[0], ":")[1]))

		return createDiagonalBlock(omegaLength)
	}

	return ParseBlockStructure(lines[1:])
}

// ParseParameterStructures parses the final estimates of model parameters from lst file
func ParseParameterStructures(lines []string) ParameterStructures {
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
	omegaStructure := parseParameterStructure(lines[omegaFormStart:omegaFormEnd])
	sigmaStructure := parseParameterStructure(lines[sigmaFormStart:sigmaFormEnd])

	return ParameterStructures{thetaLength, omegaStructure, sigmaStructure}
}
