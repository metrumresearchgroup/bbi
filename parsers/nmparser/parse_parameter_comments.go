package parser

import (
	"strings"
)

// ParameterNames containst the names of model parameters
type ParameterNames struct {
	Theta []string
	Omega []string
	Sigma []string
}

// ParseThetaComments will parse out the names from theta parameters
func ParseThetaComments(lines []string) []string {
	parsedLines := FormatThetaBlock(CleanThetaBlock(lines))
	var result = make([]string, len(parsedLines))
	for i, line := range parsedLines {
		result[i] = strings.TrimSpace(strings.Split(line, ";")[1])
	}
	return result
}

// ParseOmegaComments will parse out the omega comment names
func ParseOmegaComments(lines []string) []string {
	// TODO: implement ParseOmegaCommentss
	return make([]string, 0)
}

// ParseSigmaComments will parse out the Sigma comment names
func ParseSigmaComments(lines []string) []string {
	// TODO: implement ParseSigmaCommentss
	return make([]string, 0)
}

// ParseParameterNames parses the parameter names from the model information in the lst file
func ParseParameterNames(lines []string) ParameterNames {
	var startOmegaIndex int
	var startSigmaIndex int
	for i, line := range lines {
		switch {
		case strings.Contains(line, "$OMEGA") && startOmegaIndex == 0:
			startOmegaIndex = i
		case strings.Contains(line, "$SIGMA") && startSigmaIndex == 0:
			startSigmaIndex = i
		}
	}
	return ParameterNames{
		ParseThetaComments(lines[:startOmegaIndex]),
		ParseOmegaComments(lines[startOmegaIndex:startSigmaIndex]),
		ParseSigmaComments(lines[startSigmaIndex:]),
	}
}
