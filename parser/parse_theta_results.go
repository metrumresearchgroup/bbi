package parser

import (
	"regexp"
	"strings"
)

// ParseThetaResults parses theta results
func ParseThetaResults(lines []string) []string {
	var thetaLine string
	r := regexp.MustCompile("E[\\+|\\-]")
	for _, line := range lines {
		if r.MatchString(line) {
			thetaLine += line
		}
	}
	return strings.Fields(thetaLine)
}
