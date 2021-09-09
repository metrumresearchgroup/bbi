package parser

import (
	"strconv"
	"strings"
)

// ParseBlockStructure parses the structure of a parameter block.
func ParseBlockStructure(lines []string) []int {
	var combinedLines string
	var i int
	for _, line := range lines {
		combinedLines += line
	}
	strArr := strings.Fields(combinedLines)
	iArr := make([]int, 0, len(strArr))
	for _, str := range strArr {
		i, _ = strconv.Atoi(str)
		iArr = append(iArr, i)
	}

	return iArr
}
