package parser

import (
	"regexp"
	"strconv"
	"strings"
)

func parseFinalTime(line string) float64 {
	re := regexp.MustCompile(`[-+]?([\d]*\.[\d]+|[\d]+)$`)
	res := strToFloat(re.FindString(line))

	return res
}

func parseNMVersion(line string) string {
	re := regexp.MustCompile(`[^\s]+$`)
	res := re.FindString(line)

	return res
}

var problemRe = regexp.MustCompile(`^\s*\$PROB(?:LEM)?\s+`)

func replaceTrim(line string, replacement string) string {
	return strings.TrimSpace(strings.ReplaceAll(line, replacement, ""))
}

// TODO: remove parseValue if we find no use
/* func parseValue(line string, value string) string {
	tokens := strings.Fields(line)
	for _, s := range tokens {
		if strings.Contains(s, value) {
			return replaceTrim(s, value)
		}
	}

	return ""
} */

func parseLine(line string, n int) string {
	if tokens := strings.Fields(line); len(tokens) >= n {
		return tokens[n]
	}

	return ""
}

// ParseRunDetails parses run details such as start date/time and estimation time etc.
func ParseRunDetails(lines []string) RunDetails {
	runDetails := NewRunDetails()

	for i, line := range lines {
		lineTrimmed := strings.TrimSpace(line)
		switch {
		case strings.Contains(line, "1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION"):
			runDetails.Version = parseNMVersion(line)
		case strings.Contains(line, "NO. OF FUNCTION EVALUATIONS USED"):
			runDetails.FunctionEvaluations, _ = strconv.ParseInt(replaceTrim(line, "NO. OF FUNCTION EVALUATIONS USED:"), 10, 64)
		case strings.Contains(line, "NO. OF SIG. DIGITS IN FINAL EST.:"):
			runDetails.SignificantDigits = strToFloat(replaceTrim(line, "NO. OF SIG. DIGITS IN FINAL EST.:"))
		case strings.Contains(line, "Elapsed estimation"):
			runDetails.EstimationTime = append(runDetails.EstimationTime,
				parseFinalTime(line))
		case strings.Contains(line, "Elapsed covariance"):
			runDetails.CovarianceTime = append(runDetails.CovarianceTime,
				parseFinalTime(line))
		case strings.Contains(line, "Elapsed postprocess"):
			runDetails.PostprocessTime = parseFinalTime(line)
		case strings.Contains(line, " #CPUT: Total CPU Time in Seconds,"):
			runDetails.CpuTime = strToFloat(replaceTrim(line, " #CPUT: Total CPU Time in Seconds,"))
		case strings.Contains(line, "Started"):
			runDetails.RunStart = replaceTrim(line, "Started")
		case strings.HasPrefix(lineTrimmed, "Finished "):
			runDetails.RunEnd = strings.TrimPrefix(lineTrimmed, "Finished ")
		case lineTrimmed == "Stop Time:":
			if i+1 < len(lines) {
				runDetails.RunEnd = lines[i+1]
			}
		case problemRe.MatchString(line):
			runDetails.ProblemText = problemRe.ReplaceAllString(line, "")
		case strings.Contains(line, "#METH:"):
			runDetails.EstimationMethods = append(runDetails.EstimationMethods, replaceTrim(line, "#METH:"))
		case strings.HasPrefix(lineTrimmed, "$SIM"):
			fields := strings.SplitN(line, ";", 2)
			runDetails.OnlySim = strings.Contains(fields[0], "ONLY")
		case strings.HasPrefix(lineTrimmed, "$DATA"):
			runDetails.DataSet = parseLine(line, 1)
		case strings.Contains(line, "TOT. NO. OF INDIVIDUALS:"):
			runDetails.NumberOfSubjects, _ = strconv.ParseInt(replaceTrim(line, "TOT. NO. OF INDIVIDUALS:"), 10, 64)
		case strings.Contains(line, "TOT. NO. OF OBS RECS:"):
			runDetails.NumberOfObs, _ = strconv.ParseInt(replaceTrim(line, "TOT. NO. OF OBS RECS:"), 10, 64)
		case strings.Contains(line, "NO. OF DATA RECS IN DATA SET:"):
			runDetails.NumberOfDataRecords, _ = strconv.ParseInt(replaceTrim(line, "NO. OF DATA RECS IN DATA SET:"), 10, 64)
		// When using $INFN, the above line isn't present (see gh-227).
		case strings.Contains(line, "TOT. NO. OF DATA RECS:") && runDetails.NumberOfDataRecords == DefaultInt64:
			runDetails.NumberOfDataRecords, _ = strconv.ParseInt(replaceTrim(line, "TOT. NO. OF DATA RECS:"), 10, 64)
		// This is not reliable because TABLE statements can span multiple lines
		// TODO: support using multi-line feature, when available
		// case strings.Contains(line, "$TABLE NOPRINT ONEHEADER FILE="):
		// 	outputTable = parseValue(line, "FILE=")
		default:
			continue
		}
	}

	if runDetails.Version == "7.4.3" {
		if runDetails.RunStart == "" || runDetails.RunStart == DefaultString {
			runDetails.RunStart = lines[0]
		}
	}

	return runDetails
}
