package parser

import (
	"regexp"
	"strconv"
	"strings"
)

// RunDetails contains key information about logistics of the model run
type RunDetails struct {
	NMversion           string
	RunStart            string
	RunEnd              string
	EstimationTime      float64
	CovarianceTime      float64
	FunctionEvaluations int64
	SignificantDigits   float64
}

func parseFinalTime(line string) float64 {
	re := regexp.MustCompile("[-+]?([0-9]*\\.[0-9]+|[0-9]+)$")
	res, _ := strconv.ParseFloat(re.FindString(line), 64)
	return res
}

func parseNMVersion(line string) string {
	re := regexp.MustCompile("[^\\s]+$")
	res := re.FindString(line)
	return res
}

func replaceTrim(line string, replacement string) string {
	return strings.TrimSpace(strings.Replace(line, replacement, "", -1))
}

// ParseRunDetails parses run details such as start date/time and estimation time etc.
func ParseRunDetails(lines []string) RunDetails {
	nmversion := ""
	runStart := ""
	runEnd := ""
	estimationTime := 0.0
	covarianceTime := 0.0
	functionEvaluations := int64(0)
	significantDigits := 0.0
	for _, line := range lines {
		switch {
		case strings.Contains(line, "1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION"):
			nmversion = parseNMVersion(line)
		case strings.Contains(line, "NO. OF FUNCTION EVALUATIONS USED"):
			functionEvaluations, _ = strconv.ParseInt(replaceTrim(line, "NO. OF FUNCTION EVALUATIONS USED:"), 10, 64)
		case strings.Contains(line, "NO. OF SIG. DIGITS IN FINAL EST.:"):
			significantDigits, _ = strconv.ParseFloat(replaceTrim(line, "NO. OF SIG. DIGITS IN FINAL EST.:"), 64)
		case strings.Contains(line, "Elapsed estimation time in seconds:"):
			estimationTime = parseFinalTime(line)
		case strings.Contains(line, "Elapsed covariance time in seconds:"):
			covarianceTime = parseFinalTime(line)
		case strings.Contains(line, "Started"):
			runStart = replaceTrim(line, "Started")
		case strings.Contains(line, "Finished"):
			runEnd = replaceTrim(line, "Finished")
		default:
			continue
		}
	}

	return RunDetails{
		nmversion,
		runStart,
		runEnd,
		estimationTime,
		covarianceTime,
		functionEvaluations,
		significantDigits,
	}
}
