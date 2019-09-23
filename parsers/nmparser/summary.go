package parser

import (
	"fmt"
	"math"
	"os"
	"strconv"

	"github.com/olekukonko/tablewriter"

	"github.com/thoas/go-funk"
)

// Summary prints all results from the parsed LstData
func (results ModelOutput) Summary() bool {
	thetaTable := tablewriter.NewWriter(os.Stdout)
	thetaTable.SetAlignment(tablewriter.ALIGN_LEFT)
	thetaTable.SetColWidth(100)
	thetaTable.SetHeader([]string{"Theta", "Name", "Estimate", "StdErr (RSE)"})
	// required for color, prevents newline in row
	thetaTable.SetAutoWrapText(false)
	red := "\x1b[31;1m"
	none := "\x1b[0m"

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

		var s4 string
		if rse > 30.0 {
			s4 = fmt.Sprintf("%s%s (%s%%)%s", red, strconv.FormatFloat(seResult, 'f', -1, 64), strconv.FormatFloat(rse, 'f', 1, 64), none)
		} else {
			s4 = fmt.Sprintf("%s (%s%%)", strconv.FormatFloat(seResult, 'f', -1, 64), strconv.FormatFloat(rse, 'f', 1, 64))
		}

		thetaTable.Append([]string{
			string("TH " + strconv.Itoa(i+1)),
			results.ParameterNames.Theta[i],
			strconv.FormatFloat(numResult, 'f', -1, 64),
			s4})
	}

	omegaTable := tablewriter.NewWriter(os.Stdout)
	omegaTable.SetAlignment(tablewriter.ALIGN_LEFT)
	omegaTable.SetColWidth(100)
	omegaTable.SetHeader([]string{"Omega", "Eta", "Estimate", "ShrinkageSD (%)"})
	// required for color, prevents newline in row
	thetaTable.SetAutoWrapText(false)

	diagIndices := GetDiagonalElements(results.ParameterStructures.Omega)
	for i := range results.ParametersData[finalEstimationMethodIndex].Estimates.Omega {

		omegaIndex, _ := omegaIndices[i]
		if results.ParameterStructures.Omega[i] != 0 {
			val := results.ParametersData[finalEstimationMethodIndex].Estimates.Omega[i]
			var shrinkage float64
			var etaName string

			userEtaIndex := funk.IndexOfInt(diagIndices, i)
			if userEtaIndex > -1 {
				if len(results.ShrinkageDetails.Eta.SD) > 0 {
					shrinkage = results.ShrinkageDetails.Eta.SD[userEtaIndex]
					etaName = fmt.Sprintf("ETA%v", userEtaIndex+1)
				}
			}

			var s4 string
			if shrinkage > 30.0 {
				s4 = fmt.Sprintf("%s%f%s", red, shrinkage, none)
			} else {
				s4 = fmt.Sprintf("%f", shrinkage)
			}
			omegaTable.Append([]string{string("O" + omegaIndex), etaName, fmt.Sprintf("%f", val), s4})
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

	thetaTable.Render()
	omegaTable.Render()
	return true
}
