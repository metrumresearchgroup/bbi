package parser

import "strings"

// AddPathLevelToData adds a level to the path declared in $DATA
func AddPathLevelToData(s string) string {
	dataComponents := strings.Fields(s)
	// path should be second field
	originalPath := dataComponents[1]
	if strings.HasPrefix(originalPath, "/") {
		// don't change if set to absolute path already
		return (s)
	}
	newPath := strings.Join([]string{
		"../",
		originalPath,
	}, "")
	return strings.Join([]string{
		"$DATA",
		newPath,
	}, " ")
}
