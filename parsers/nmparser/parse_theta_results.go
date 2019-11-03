package parser

import (
	"regexp"
	"strconv"
	"strings"
)

// ParseThetaResults parses theta results
func ParseThetaResults(lines []string) []float64 {
	var thetaLine string
	var thetaValues []float64
	r := regexp.MustCompile("E[\\+|\\-]")
	for _, line := range lines {
		if r.MatchString(line) {
			thetaLine += line
		}
	}
	stringValues := strings.Fields(thetaLine)
	for _, strVal := range stringValues {
		parsedVal, err := strconv.ParseFloat(strVal, 64)
		if err != nil {
			parsedVal = DefaultFloat64
		}
		thetaValues = append(thetaValues, parsedVal)
	}
	return thetaValues
}
