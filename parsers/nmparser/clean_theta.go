package parser

import "strings"

// CleanThetaBlock will clean information from the theta block to prepare for additional parsingor reformating
func CleanThetaBlock(lines []string) []string {
	var result = make([]string, 0)
	for _, line := range lines {
		cleaned := strings.TrimSpace(strings.Replace(line, "$THETA", "", -1))
		// remove any comment only lines at this point
		if cleaned == "" || string(cleaned[0]) == ";" {
			continue
		}
		result = append(result, cleaned)
	}
	return result
}
