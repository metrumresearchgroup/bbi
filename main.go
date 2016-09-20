package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"

	"github.com/dpastoor/nonmemutils/parser"
)

func main() {
	data, _ := readLine("parser/fixtures/lstfiles/simple-onecmpt-ex1.lst")
	results := parser.ParseLstEstimationFile(data)
	bs, _ := json.Marshal(results)
	fmt.Println(string(bs))
	results.Summary()
}

func readLine(path string) ([]string, error) {
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
