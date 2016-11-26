package parser

import "strings"
import "path/filepath"

// AddPathLevelToData adds a level to the path declared in $DATA
func AddPathLevelToData(s string) string {
	dataComponents := strings.Fields(s)
	// path should be second field
	originalPath := dataComponents[1]
	if strings.HasPrefix(originalPath, "/") {
		// don't change if set to absolute path already
		return (s)
	}
	newPath := filepath.Join(
		"..",
		originalPath,
	)
	return strings.Join([]string{
		"$DATA",
		newPath,
	}, " ")
}
