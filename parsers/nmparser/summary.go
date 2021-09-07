package parser

import (
	"fmt"
	"math"
	"os"
	"strconv"

	"github.com/logrusorgru/aurora"
	"github.com/olekukonko/tablewriter"
)

// Summary prints all results from the parsed LstData.
func (results SummaryOutput) Summary() bool {
	thetaTable := tablewriter.NewWriter(os.Stdout)
	thetaTable.SetAlignment(tablewriter.ALIGN_LEFT)
	thetaTable.SetColWidth(100)
	thetaTable.SetHeader([]string{"Theta", "Name", "Estimate", "StdErr (RSE)"})
	// required for color, prevents newline in row
	thetaTable.SetAutoWrapText(false)
	//isBayesian := CheckIfBayesian(results)
	shk := results.ShrinkageDetails != nil
	finalEstimationMethodIndex := len(results.ParametersData) - 1
	for i := range results.ParametersData[finalEstimationMethodIndex].Estimates.Theta {
		numResult := results.ParametersData[finalEstimationMethodIndex].Estimates.Theta[i]
		seResult := results.ParametersData[finalEstimationMethodIndex].StdErr.Theta[i]
		fixed := results.ParametersData[finalEstimationMethodIndex].Fixed.Theta[i]
		var rse float64
		if seResult != -999999999 && numResult != 0 && seResult != DefaultFloat64 && numResult != DefaultFloat64 {
			rse = math.Abs(seResult / numResult * 100)
		}

		var s4 string
		if fixed == 1 {
			s4 = "FIX"
		} else if seResult == -999999999 {
			s4 = "-"
		} else if rse > 30.0 {
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
	omegaHeaders := []string{"Omega", "Eta", "Estimate"}
	if shk {
		// bayesian methods have no concept of shrinkage
		nSubPopsForMixtureModels := len(results.ShrinkageDetails[len(results.RunDetails.EstimationMethods)-1])
		if nSubPopsForMixtureModels > 1 {
			for i := 0; i < nSubPopsForMixtureModels; i++ {
				omegaHeaders = append(omegaHeaders, fmt.Sprintf("Pop%v Shrinkage (%%)", i+1))
			}
		} else {
			// single population
			omegaHeaders = append(omegaHeaders, "Shrinkage (%)")
		}
	}
	omegaTable.SetHeader(omegaHeaders)
	// required for color, prevents newline in row
	thetaTable.SetAutoWrapText(false)

	methodIndex := len(results.RunDetails.EstimationMethods) - 1
	for n := range results.ParametersData[finalEstimationMethodIndex].Estimates.Omega {
		diagIndex, isDiag := IndexAndIsDiag(n)
		if !isDiag {
			continue
		}
		var shrinkageValues []string
		etaName := "-"
		val := results.ParametersData[finalEstimationMethodIndex].Estimates.Omega[n]
		if isDiag {
			etaName = fmt.Sprintf("ETA%v", diagIndex)
			if shk {
				for sp := range results.ShrinkageDetails[methodIndex] {
					if len(results.ShrinkageDetails[methodIndex]) > 0 {
						// get the data for the last method
						shrinkageDetails := results.ShrinkageDetails[len(results.ShrinkageDetails)-1]
						shrinkage := shrinkageDetails[sp].EtaSD[diagIndex-1]
						if shrinkage > 30.0 {
							shrinkageValues = append(shrinkageValues, aurora.Sprintf(aurora.Red("%s"), fmt.Sprintf("%f", shrinkage)))
						} else {
							shrinkageValues = append(shrinkageValues, fmt.Sprintf("%f", shrinkage))
						}
					}
				}
			}
		}
		omegaTable.Append(append([]string{results.ParameterNames.Omega[n], etaName, fmt.Sprintf("%f", val)}, shrinkageValues...))
	}

	fmt.Println(results.RunDetails.ProblemText)
	fmt.Println("Dataset: " + results.RunDetails.DataSet)
	fmt.Println(fmt.Sprintf("Records: %v   Observations: %v  Subjects: %v",
		results.RunDetails.NumberOfDataRecords,
		results.RunDetails.NumberOfObs,
		results.RunDetails.NumberOfSubjects,
	))
	fmt.Println("Estimation Method(s):")
	for _, em := range results.RunDetails.EstimationMethods {
		fmt.Println(" - " + em)
	}
	if results.RunHeuristics.AnyTrue() {
		fmt.Println(aurora.Bold(aurora.Red("Heuristic Problems Detected: ")))
		issues := results.RunHeuristics.ErrorStrings()
		for _, issue := range issues {
			fmt.Println(aurora.Red(" - " + issue))
		}
	} else {
		fmt.Println("No Heuristic Problems Detected")
	}

	thetaTable.Render()
	omegaTable.Render()

	return true
}
