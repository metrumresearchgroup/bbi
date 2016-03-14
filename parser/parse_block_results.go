package parser

import (
	"regexp"
	"strings"
)

// ParseOmegaResults parses omega results
func ParseBlockResults(lines []string) []string {
	var omegaLine string
	r := regexp.MustCompile("E[\\+|\\-]")
	for _, line := range lines {
		if r.MatchString(line) {
			// slice off the leading + from each line
			omegaLine += line[1:]
		}
	}
	return strings.Fields(omegaLine)
}
