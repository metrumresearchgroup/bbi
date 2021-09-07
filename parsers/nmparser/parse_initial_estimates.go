package parser

import "strings"

// InitialEstimates contains information about intitial estimates and boundary conditions
type InitialEstimates struct {
	Theta []IETheta
	Omega []IERandomEffect
	Sigma []IERandomEffect
}

// IETheta represents initial estimate for theta with boundaries and (guess) of whether fixed (all values same)
type IETheta struct {
	LB    string
	IE    string
	UB    string
	Fixed bool
}

// IERandomEffect represents initial estimate for random effects for omega and sigma
type IERandomEffect struct {
	Value string
	Fixed bool
}

func parseInitialThetas(lines []string) []IETheta {
	// skip Lower Initial Upper bound deliniation line

	// strip any annotations
	return []IETheta{IETheta{"0", "0", "0", true}}
}

// ParseBlockStructure parses the structure of a parameter block
func parseSimpleBlockStructure(lines []string) []IERandomEffect {
	var combinedLines string
	for _, line := range lines {
		// only lines in the block with values are indented
		if strings.HasPrefix(line, " ") {
			combinedLines += line
		}
	}
	values := strings.Fields(combinedLines)
	results := make([]IERandomEffect, len(values))
	for i, val := range values {
		results[i] = IERandomEffect{val, false}
	}

	return results
}

func parseInitialOmegas(lines []string) []IERandomEffect {
	// TODO: handle complex block structures in Omega
	return parseSimpleBlockStructure(lines)
}

func parseInitialSigmas(lines []string) []IERandomEffect {
	return parseSimpleBlockStructure(lines)
}

// ParseInitialEstimates parses the initial estimate lines of the lst file
func ParseInitialEstimates(lines []string) InitialEstimates {
	var initialThetaIndex int
	var initialOmegaIndex int
	var initialSigmaIndex int
	for i, line := range lines {
		switch {
		case strings.Contains(line, "0INITIAL ESTIMATE OF THETA"):
			initialThetaIndex = i
		case strings.Contains(line, "0INITIAL ESTIMATE OF OMEGA"):
			initialOmegaIndex = i
		case strings.Contains(line, "0INITIAL ESTIMATE OF SIGMA"):
			initialSigmaIndex = i
		default:
			continue
		}

	}

	result := InitialEstimates{
		parseInitialThetas(lines[(initialThetaIndex + 1):initialOmegaIndex]),
		parseInitialOmegas(lines[(initialOmegaIndex + 1):initialSigmaIndex]),
		parseInitialSigmas(lines[(initialSigmaIndex + 1):]),
	}

	return result
}
