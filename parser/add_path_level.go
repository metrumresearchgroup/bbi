package parser

import (
	"fmt"
	"path/filepath"
	"strings"
)

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
	// add things after the path back in if they exist
	if len(dataComponents) > 2 {
		newPath = fmt.Sprintf(
			"%s %s",
			newPath,
			strings.Join(dataComponents[2:], " "),
		)
	}
	return strings.Join([]string{
		"$DATA",
		newPath,
	}, " ")
}
