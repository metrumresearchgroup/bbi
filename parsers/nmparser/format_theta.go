package parser

import "strings"

// FormatThetaBlock will format each line in the theta block
func FormatThetaBlock(lines []string) []string {
	var result = make([]string, 0)
	longestCommentIndex := 0

	for _, line := range lines {
		lineIndex := strings.Index(line, ";")
		// some thetas may have no comments and would want to inject comments after
		if lineIndex == -1 {
			lineIndex = len(line) + 1
		}
		if lineIndex > longestCommentIndex {
			longestCommentIndex = lineIndex
		}
	}
	// add two extra spaces for more padding
	// longestLine = longestLine + 2
	for _, line := range lines {
		commentIndex := strings.Index(line, ";")
		var theta string
		var comment string
		if commentIndex == -1 {
			theta = line
			comment = strings.Repeat(" ", longestCommentIndex-len(line)) + "; <NEED COMMENT>"
		} else {
			theta = line[:commentIndex]
			comment = strings.Repeat(" ", longestCommentIndex-len(theta)) + line[commentIndex:]
		}
		result = append(result, theta+comment)
	}

	return result
}
