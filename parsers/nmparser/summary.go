package parser

import (
	"fmt"
	"math"
	"strconv"

	"github.com/apcera/termtables"
	"github.com/logrusorgru/aurora"
	"github.com/thoas/go-funk"
)

// Summary prints all results from the parsed LstData
func (results ModelOutput) Summary() bool {
	termtables.DefaultStyle = &termtables.TableStyle{
		SkipBorder: false,
		BorderX:    "-", BorderY: "|", BorderI: "+",
		PaddingLeft: 3, PaddingRight: 3,
		Width:     100,
		Alignment: termtables.AlignRight,
	}
	thetaTable := termtables.CreateTable()
	thetaTable.AddHeaders("Theta", "Name", "Estimate", "StdErr (RSE)")
	finalEstimationMethodIndex := len(results.ParametersData) - 1
	if len(results.ParametersData[finalEstimationMethodIndex].Estimates.Theta) != len(results.ParametersData[finalEstimationMethodIndex].StdErr.Theta) {
		// if the standard errors aren't there, we should
		// instead make an equal length slice so that looping to build the table won't blow
		// up with an index out of bounds error
		results.ParametersData[finalEstimationMethodIndex].StdErr.Theta = make([]float64, len(results.ParametersData[finalEstimationMethodIndex].Estimates.Theta))
	}
	for i := range results.ParametersData[finalEstimationMethodIndex].Estimates.Theta {
		numResult := results.ParametersData[finalEstimationMethodIndex].Estimates.Theta[i]
		seResult := results.ParametersData[finalEstimationMethodIndex].StdErr.Theta[i]
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
					fmt.Sprintf("%s (%s%%)",
						strconv.FormatFloat(seResult, 'f', -1, 64),
						strconv.FormatFloat(rse, 'f', 1, 64)),
				),
			)
		} else {
			thetaTable.AddRow(
				"TH "+strconv.Itoa(i+1),
				results.ParameterNames.Theta[i],
				strconv.FormatFloat(numResult, 'f', -1, 64),
				fmt.Sprintf("%s (%s%%)",
					strconv.FormatFloat(seResult, 'f', -1, 64),
					strconv.FormatFloat(rse, 'f', 1, 64),
				),
			)

		}

	}
	thetaTable.SetAlign(termtables.AlignLeft, 1)

	omegaTable := termtables.CreateTable()
	omegaTable.AddHeaders("Omega", "Eta", "Estimate", "ShrinkageSD (%)")
	diagIndices := GetDiagonalElements(results.ParameterStructures.Omega)
	for i := range results.ParametersData[finalEstimationMethodIndex].Estimates.Omega {
		omegaIndex, _ := omegaIndices[i]
		if results.ParameterStructures.Omega[i] != 0 {
			val := results.ParametersData[finalEstimationMethodIndex].Estimates.Omega[i]
			var shrinkage float64
			var etaName string
			userEtaIndex := funk.IndexOfInt(diagIndices, i)
			if userEtaIndex > -1 {
				shrinkage = results.ShrinkageDetails.Eta.SD[userEtaIndex]
				etaName = fmt.Sprintf("ETA%v", userEtaIndex+1)
			}
			if shrinkage > 30 {
				omegaTable.AddRow(
					aurora.Red("O"+omegaIndex),
					aurora.Red(etaName),
					aurora.Red(val),
					aurora.Red(shrinkage),
				)
			} else {
				omegaTable.AddRow(
					"O"+omegaIndex,
					etaName,
					//"ETA "+strconv.Itoa(userEtaIndex+1),
					val,
					shrinkage,
				)
			}
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
