package parser

import (
	"regexp"
	"strconv"
	"strings"
)

// ParseBlockResults parses results stored in a block-line structure
func ParseBlockResults(lines []string) []float64 {
	var omegaLine string
	var blockValues []float64
	r := regexp.MustCompile("E[\\+|\\-]")
	for _, line := range lines {
		if r.MatchString(line) || strings.Contains(line, ".........") {
			// slice off the leading + from each line
			omegaLine += line[1:]
		}
	}
	stringValues := strings.Fields(omegaLine)
	for _, strVal := range stringValues {
		parsedVal, _ := strconv.ParseFloat(strVal, 64)
		blockValues = append(blockValues, parsedVal)
	}
	return blockValues
}
