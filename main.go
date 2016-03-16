package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strconv"

	"github.com/apcera/termtables"
	"github.com/dpastoor/nonmemutils/parser"
	"github.com/fatih/color"
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

	thetaTable := termtables.CreateTable()
	thetaTable.AddHeaders("Theta", "Name", "Estimate")
	for i := range results.FinalParameterEstimates.Theta {
		if i == 2 {
			thetaTable.AddRow(
				color.RedString("TH "+strconv.Itoa(i+1)),
				color.RedString(results.ParameterNames.Theta[i]),
				color.RedString(results.FinalParameterEstimates.Theta[i]),
			)
		} else {
			thetaTable.AddRow(
				"TH "+strconv.Itoa(i+1),
				results.ParameterNames.Theta[i],
				results.FinalParameterEstimates.Theta[i],
			)

		}

	}
	thetaTable.SetAlign(termtables.AlignLeft, 1)

	omegaTable := termtables.CreateTable()
	omegaTable.AddHeaders("Omega", "Estimate")
	// for i := range results.FinalParameterEstimates.Omega {
	// 	omegaTable.AddRow(results.ParameterNames.Omega[i], results.FinalParameterEstimates.Omega[i])
	// }
	omegaTable.SetAlign(termtables.AlignLeft, 1)

	fmt.Println(thetaTable.Render())
	fmt.Println(omegaTable.Render())
}
