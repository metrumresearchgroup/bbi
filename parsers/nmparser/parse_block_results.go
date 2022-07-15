package parser

import (
	"regexp"
	"strings"
)

// ParseBlockResults parses results stored in a block-line structure.
func ParseBlockResults(lines []string) []float64 {
	if len(lines) == 0 {
		return []float64{}
	}
	var omegaLine string
	var blockValues []float64
	r := regexp.MustCompile(`E[+-]`)
	for _, line := range lines {
		if r.MatchString(line) || strings.Contains(line, ".........") {
			// slice off the leading + from each line
			omegaLine += line[1:]
		}
	}
	stringValues := strings.Fields(omegaLine)
	for _, strVal := range stringValues {
		parsedVal := strToFloat(strVal)
		blockValues = append(blockValues, parsedVal)
	}

	return blockValues
}
