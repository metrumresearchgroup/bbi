package runner

import (
	"strings"

	parser "github.com/metrumresearchgroup/bbi/parsers/nmparser"
)

// PrepareForExecution parses and prepares strings from a file for execution in a different context
// for example, replacing the $DATA path.
func PrepareForExecution(s []string) []string {
	for i, line := range s {
		if strings.Contains(line, "$DATA") {
			s[i] = parser.AddPathLevelToData(line)
			// assuming only one $DATA record can just fix this and exit
			break
		}
	}

	return s
}
