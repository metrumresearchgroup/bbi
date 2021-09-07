package parser

import "regexp"

// FindOutputFiles will return all files defined with the pattern FILE=<FILE>
// in the lines provided
func FindOutputFiles(lines []string) []string {
	files := []string{}
	r, _ := regexp.Compile("FILE\\s?=\\s?(\\S+)")
	for _, line := range lines {
		match := r.FindStringSubmatch(line)
		if len(match) > 0 {
			files = append(files, match[1])
		}
	}

	return files
}
