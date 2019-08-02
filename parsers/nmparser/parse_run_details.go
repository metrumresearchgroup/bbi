package parser

import (
	"regexp"
	"strconv"
	"strings"
)

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

func parseTableFile(lines []string) string {
	for _, line := range lines {
		if strings.Contains(line, "FILE=") {
			return parseValue(line, "FILE=")
		}
	}
	return ""
}

func replaceTrim(line string, replacement string) string {
	return strings.TrimSpace(strings.Replace(line, replacement, "", -1))
}

func parseValue(line string, value string) string {
	tokens := strings.Fields(line)
	for _, s := range tokens {
		if strings.Contains(s, value) {
			return replaceTrim(s, value)
		}
	}
	return ""
}

func parseLine(line string, n int) string {
	tokens := strings.Fields(line)
	if len(tokens) >= n {
		return tokens[n]
	}
	return ""
}

func getLines(lines []string, index int, count int) []string {
	size := len(lines)
	if index < 0 {
		return []string{}
	}
	if index >= size {
		return []string{}
	}
	if count >= 0 {
		end := index + count
		if end > size {
			end = size
		}
		return lines[index:end]
	}
	if count < 0 {
		start := index + count + 1
		if start < 0 {
			start = 0
		}
		return lines[start : index+1]
	}
	return []string{}
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
	problemText := ""
	modFile := "" // TODO, pass in from caller
	estimationMethod := []string{}
	dataSet := ""
	numberOfPatients := int64(0)
	numberOfObs := int64(0)
	numberOfDataRecords := int64(0)
	outputTables := []string{}

	for i, line := range lines {
		switch {
		case strings.Contains(line, "1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION"):
			nmversion = parseNMVersion(line)
		case strings.Contains(line, "NO. OF FUNCTION EVALUATIONS USED"):
			functionEvaluations, _ = strconv.ParseInt(replaceTrim(line, "NO. OF FUNCTION EVALUATIONS USED:"), 10, 64)
		case strings.Contains(line, "NO. OF SIG. DIGITS IN FINAL EST.:"):
			significantDigits, _ = strconv.ParseFloat(replaceTrim(line, "NO. OF SIG. DIGITS IN FINAL EST.:"), 64)
		case strings.Contains(line, "Elapsed estimation"):
			estimationTime = parseFinalTime(line)
		case strings.Contains(line, "Elapsed covariance time in seconds:"):
			covarianceTime = parseFinalTime(line)
		case strings.Contains(line, "Elapsed postprocess time in seconds:"):
			covarianceTime = parseFinalTime(line)
		case strings.Contains(line, "Started"):
			runStart = replaceTrim(line, "Started")
		case strings.Contains(line, "Finished"):
			runEnd = replaceTrim(line, "Finished")
		case strings.Contains(line, "Stop Time:"):
			if i+1 < len(lines) {
				runEnd = lines[i+1]
			}
		case strings.Contains(line, "$PROB"):
			problemText = replaceTrim(line, "$PROB")
		case strings.Contains(line, "#METH:"):
			estimationMethod = append(estimationMethod, replaceTrim(line, "#METH:"))
		case strings.Contains(line, "$DATA"):
			dataSet = parseLine(line, 1)
		case strings.Contains(line, "TOT. NO. OF INDIVIDUALS:"):
			numberOfPatients, _ = strconv.ParseInt(replaceTrim(line, "TOT. NO. OF INDIVIDUALS:"), 10, 64)
		case strings.Contains(line, "TOT. NO. OF OBS RECS:"):
			numberOfObs, _ = strconv.ParseInt(replaceTrim(line, "TOT. NO. OF OBS RECS:"), 10, 64)
		case strings.Contains(line, "NO. OF DATA RECS IN DATA SET:"):
			numberOfDataRecords, _ = strconv.ParseInt(replaceTrim(line, "NO. OF DATA RECS IN DATA SET:"), 10, 64)
		case strings.Contains(line, "$TABLE"):
			outputTables = append(outputTables, parseTableFile(getLines(lines, i, 10)))
		default:
			continue
		}
	}

	if nmversion == "7.4.3" {
		if runStart == "" {
			runStart = lines[0]
		}
	}

	return RunDetails{
		NMversion:           nmversion,
		RunStart:            runStart,
		RunEnd:              runEnd,
		EstimationTime:      estimationTime,
		CovarianceTime:      covarianceTime,
		FunctionEvaluations: functionEvaluations,
		SignificantDigits:   significantDigits,
		ProblemText:         problemText,
		ModFile:             modFile,
		EstimationMethod:    estimationMethod,
		DataSet:             dataSet,
		NumberOfPatients:    numberOfPatients,
		NumberOfObs:         numberOfObs,
		NumberOfDataRecords: numberOfDataRecords,
		OutputTables:        outputTables,
	}
}
