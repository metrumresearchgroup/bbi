package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"

	"github.com/apcera/termtables"
	"github.com/dpastoor/nonmemutils/parser"
)

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
func main() {
	data, _ := readLine("parser/fixtures/lstfiles/simple-onecmpt-ex1.lst")
	results := parser.ParseLstEstimationFile(data)
	bs, _ := json.Marshal(results)
	fmt.Println(string(bs))
	termtables.DefaultStyle = &termtables.TableStyle{
		SkipBorder: false,
		BorderX:    "-", BorderY: "|", BorderI: "+",
		PaddingLeft: 3, PaddingRight: 3,
		Width:     80,
		Alignment: termtables.AlignRight,
	}
	table := termtables.CreateTable()
	table.AddHeaders("Name", "Estimate")
	for i := range results.FinalParameterEstimates.Theta {
		table.AddRow(results.ParameterNames.Theta[i], results.FinalParameterEstimates.Theta[i])
	}
	table.SetAlign(termtables.AlignLeft, 1)

	fmt.Println(table.Render())
}
