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
	thetaTable.AddHeaders("Theta", "Name", "Estimate", "StdErr (RSE)")
	for i := range results.FinalParameterEstimates.Theta {
		numResult := results.FinalParameterEstimates.Theta[i]
		seResult := results.FinalParameterStdErr.Theta[i]
		var rse float64
		if seResult != 0 && numResult != 0 {
			rse = math.Abs(seResult / numResult * 100)

		}

		if rse > 30 {
			//theta, _ := fmt.Printf("%.3E", results.FinalParameterEstimates.Theta[i])
			thetaTable.AddRow(
				aurora.Red("TH "+strconv.Itoa(i+1)),
				aurora.Red(results.ParameterNames.Theta[i]),
				aurora.Red(strconv.FormatFloat(numResult, 'f', -1, 64)),
				aurora.Red(
					fmt.Sprintf("%s (%s %%)",
						strconv.FormatFloat(seResult, 'f', -1, 64),
						strconv.FormatFloat(rse, 'f', 1, 64)),
				),
			)
		} else {
			thetaTable.AddRow(
				"TH "+strconv.Itoa(i+1),
				results.ParameterNames.Theta[i],
				strconv.FormatFloat(numResult, 'f', -1, 64),
				fmt.Sprintf("%s (%s %%)",
					strconv.FormatFloat(seResult, 'f', -1, 64),
					strconv.FormatFloat(rse, 'f', 1, 64),
				),
			)

		}

	}
	thetaTable.SetAlign(termtables.AlignLeft, 1)

	omegaTable := termtables.CreateTable()
	omegaTable.AddHeaders("Omega", "Estimate", "ShrinkageSD (%)")
	userEta := 0
	for i := range results.FinalParameterEstimates.Omega {
		if results.ParameterStructures.Omega[i] != 0 {
			userEta++
			val := results.FinalParameterEstimates.Omega[i]
			omegaTable.AddRow("ETA "+strconv.Itoa(userEta),
				val,
				results.ShrinkageDetails.Eta.SD[userEta-1],
			)
		}

	}
	fmt.Println(results.RunDetails.ProblemText)
	fmt.Println("Dataset: " + results.RunDetails.DataSet)
	fmt.Println(fmt.Sprintf("Records: %v   Observations: %v  Patients: %v",
		results.RunDetails.NumberOfDataRecords,
		results.RunDetails.NumberOfObs,
		results.RunDetails.NumberOfPatients,
	))
	fmt.Println("Estimation Method(s):")
	for _, em := range results.RunDetails.EstimationMethod {
		fmt.Println(" - " + em)
	}
	omegaTable.SetAlign(termtables.AlignLeft, 1)
	fmt.Println(thetaTable.Render())
	fmt.Println(omegaTable.Render())
	return true
}
