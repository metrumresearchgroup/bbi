package utils

import (
	"bufio"
	"os"
)

//ReadLines reads lines for a file at a given path
func ReadLines(path string) ([]string, error) {
	inFile, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer inFile.Close()
	scanner := bufio.NewScanner(inFile)
	scanner.Split(bufio.ScanLines)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines, nil
}
