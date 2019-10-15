package parser

import (
	"fmt"
	"math"
	"os"
	"strconv"

	"github.com/logrusorgru/aurora"
	"github.com/olekukonko/tablewriter"
)

// Summary prints all results from the parsed LstData
func (results ModelOutput) Summary() bool {
	thetaTable := tablewriter.NewWriter(os.Stdout)
	thetaTable.SetAlignment(tablewriter.ALIGN_LEFT)
	thetaTable.SetColWidth(100)
	thetaTable.SetHeader([]string{"Theta", "Name", "Estimate", "StdErr (RSE)"})
	// required for color, prevents newline in row
	thetaTable.SetAutoWrapText(false)

	finalEstimationMethodIndex := len(results.ParametersData) - 1
	for i := range results.ParametersData[finalEstimationMethodIndex].Estimates.Theta {
		numResult := results.ParametersData[finalEstimationMethodIndex].Estimates.Theta[i]
		seResult := results.ParametersData[finalEstimationMethodIndex].StdErr.Theta[i]
		var rse float64
		if seResult != 0 && numResult != 0 {
			rse = math.Abs(seResult / numResult * 100)
		}

		var s4 string
		if rse > 30.0 {
			s4 = aurora.Sprintf(aurora.Red("%s"), fmt.Sprintf("%s (%s%%)", strconv.FormatFloat(seResult, 'f', -1, 64), strconv.FormatFloat(rse, 'f', 1, 64)))
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

	diagIndices := GetDiagonalIndices(results.ParameterStructures.Omega)
	for n, omegaIndex := range diagIndices {
		//shrinkageValue := "0"
		var shrinkageValue string
		//val := 0.0
		var val float64
		if len(results.ShrinkageDetails) > 0 {
			// get the data for the last method
			shrinkageDetails := results.ShrinkageDetails[len(results.ShrinkageDetails)-1]
			if n < len(shrinkageDetails.Eta.SD) {
				shrinkage := shrinkageDetails.Eta.SD[n]
				if shrinkage > 30.0 {
					shrinkageValue = aurora.Sprintf(aurora.Red("%s"), fmt.Sprintf("%f", shrinkage))
				} else {
					shrinkageValue = fmt.Sprintf("%f", shrinkage)
				}
			}
			val = results.ParametersData[finalEstimationMethodIndex].Estimates.Omega[omegaIndex]
		}

		etaName := fmt.Sprintf("ETA%v", n+1)
		omegaIndices := fmt.Sprintf("(%s,%s)", strconv.Itoa(n), strconv.Itoa(n))
		omegaTable.Append([]string{string("O" + omegaIndices), etaName, fmt.Sprintf("%f", val), shrinkageValue})
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
