package parser

import (
	"fmt"
	"strconv"

	"github.com/apcera/termtables"
	"github.com/fatih/color"
)

// Summary prints all results from the parsed LstData
func (results LstData) Summary() bool {
	termtables.DefaultStyle = &termtables.TableStyle{
		SkipBorder: false,
		BorderX:    "-", BorderY: "|", BorderI: "+",
		PaddingLeft: 3, PaddingRight: 3,
		Width:     100,
		Alignment: termtables.AlignRight,
	}
	thetaTable := termtables.CreateTable()
	thetaTable.AddHeaders("Theta", "Name", "Estimate (SN)", "Estimate", "StdErr")
	for i := range results.FinalParameterEstimates.Theta {
		numResult, _ := strconv.ParseFloat(results.FinalParameterEstimates.Theta[i], 64)
		if i == 2 {
			thetaTable.AddRow(
				color.RedString("TH "+strconv.Itoa(i+1)),
				color.RedString(results.ParameterNames.Theta[i]),
				color.RedString(results.FinalParameterEstimates.Theta[i]),
				color.RedString(strconv.FormatFloat(numResult, 'f', -1, 64)),
				color.RedString("-"),
			)
		} else {
			thetaTable.AddRow(
				"TH "+strconv.Itoa(i+1),
				results.ParameterNames.Theta[i],
				results.FinalParameterEstimates.Theta[i],
				strconv.FormatFloat(numResult, 'f', -1, 64),
				"-",
			)

		}

	}
	thetaTable.SetAlign(termtables.AlignLeft, 1)
	thetaTable.SetAlign(termtables.AlignLeft, 5)

	omegaTable := termtables.CreateTable()
	omegaTable.AddHeaders("Omega", "Estimate")
	userEta := 0
	for i := range results.FinalParameterEstimates.Omega {
		if results.ParameterStructures.Omega[i] != 0 {
			userEta++
			val, _ := strconv.ParseFloat(results.FinalParameterEstimates.Omega[i], 64)
			omegaTable.AddRow("ETA "+strconv.Itoa(userEta), val)
		}

	}
	omegaTable.SetAlign(termtables.AlignLeft, 1)

	fmt.Println(thetaTable.Render())
	fmt.Println(omegaTable.Render())
	return true
}
