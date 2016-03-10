package main

import (
	"bufio"
	"fmt"
	"os"
)

// readLines reads a whole file into memory
// and returns a slice of its lines.
func readLines(path string) ([]string, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines, scanner.Err()
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
func main() {
	lines, err := readLines("test/fixtures/blocks/theta-block1.lst")
	check(err)
	for i, line := range lines {
		fmt.Println(i, line)
	}
}
