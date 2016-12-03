package parser

import (
	"fmt"
	"math"
	"strconv"

	"github.com/apcera/termtables"
	"github.com/logrusorgru/aurora"
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
	thetaTable.AddHeaders("Theta", "Name", "Estimate (SN)", "Estimate", "StdErr", "SE (RSE)")
	for i := range results.FinalParameterEstimates.Theta {
		numResult := results.FinalParameterEstimates.Theta[i]
		seResult := results.FinalParameterStdErr.Theta[i]
		var rse float64
		if seResult != 0 && numResult != 0 {
			rse = math.Abs(seResult / numResult * 100)

		}

		if i == 2 {
			//theta, _ := fmt.Printf("%.3E", results.FinalParameterEstimates.Theta[i])
			thetaTable.AddRow(
				aurora.Red("TH "+strconv.Itoa(i+1)),
				aurora.Red(results.ParameterNames.Theta[i]),
				aurora.Red(strconv.FormatFloat(numResult, 'E', 2, 64)),
				aurora.Red(strconv.FormatFloat(numResult, 'f', -1, 64)),
				aurora.Red(strconv.FormatFloat(seResult, 'f', -1, 64)),
				aurora.Red(fmt.Sprintf("%s %%", strconv.FormatFloat(rse, 'f', 1, 64))),
			)
		} else {
			thetaTable.AddRow(
				"TH "+strconv.Itoa(i+1),
				results.ParameterNames.Theta[i],
				strconv.FormatFloat(numResult, 'E', 2, 64),
				strconv.FormatFloat(numResult, 'f', -1, 64),
				strconv.FormatFloat(seResult, 'f', -1, 64),
				fmt.Sprintf("%s %%", strconv.FormatFloat(rse, 'f', 1, 64)),
			)

		}

	}
	thetaTable.SetAlign(termtables.AlignLeft, 1)

	omegaTable := termtables.CreateTable()
	omegaTable.AddHeaders("Omega", "Estimate")
	userEta := 0
	for i := range results.FinalParameterEstimates.Omega {
		if results.ParameterStructures.Omega[i] != 0 {
			userEta++
			val := results.FinalParameterEstimates.Omega[i]
			omegaTable.AddRow("ETA "+strconv.Itoa(userEta), val)
		}

	}
	omegaTable.SetAlign(termtables.AlignLeft, 1)
	fmt.Println(thetaTable.Render())
	fmt.Println(omegaTable.Render())
	return true
}
