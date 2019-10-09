package parser

import (
	"log"
	"path/filepath"
	"strings"

	"github.com/metrumresearchgroup/babylon/utils"
	"github.com/spf13/afero"
)

// GetModelOutput populates and returns a ModelOutput object by parsing files
// ParameterData is parsed from the ext file when useExtFile is true
// ParameterData is parsed from the lst file when useExtFile is false
func GetModelOutput(filePath string, verbose bool, useExt bool, useGrd bool, useCoX bool) ModelOutput {

	AppFs := afero.NewOsFs()
	runNum, _ := utils.FileAndExt(filePath)
	dir, _ := filepath.Abs(filepath.Dir(filePath))
	outputFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".lst"}, "")

	if verbose {
		log.Printf("base dir: %s", dir)
	}

	fileLines, _ := utils.ReadLinesFS(AppFs, outputFilePath)
	results := ParseLstEstimationFile(fileLines)
	results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, outputFilePath)

	if useExt {
		extFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".ext"}, "")
		extLines, err := utils.ReadParamsAndOutputFromExt(extFilePath)
		if err != nil {
			panic(err)
		}
		extData, _ := ParseExtData(ParseExtLines(extLines))
		results.ParametersData = extData
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, extFilePath)
	}

	if useGrd {
		grdFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".grd"}, "")
		grdLines, err := utils.ReadParamsAndOutputFromExt(grdFilePath)
		if err != nil {
			panic(err)
		}
		parametersData, _ := ParseGrdData(ParseGrdLines(grdLines))
		results.RunHeuristics.HasFinalZeroGradient = HasZeroGradient(parametersData[len(parametersData)-1].Fixed.Theta)
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, grdFilePath)
	}

	if useCoX {
		covFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".cov"}, "")
		covLines, err := utils.ReadLines(covFilePath)
		if err == nil {
			results.CovarianceTheta = GetThetaValues(covLines)
		}

		corFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".cor"}, "")
		corLines, err := utils.ReadLines(corFilePath)
		if err == nil {
			results.CorrelationTheta = GetThetaValues(corLines)
		}
	}

	for i := range results.ParametersData {
		if len(results.ParametersData[i].Estimates.Theta) != len(results.ParametersData[i].StdErr.Theta) {
			results.ParametersData[i].StdErr.Theta = make([]float64, len(results.ParametersData[i].Estimates.Theta))
		}
		if len(results.ParametersData[i].Estimates.Omega) != len(results.ParametersData[i].StdErr.Omega) {
			results.ParametersData[i].StdErr.Omega = make([]float64, len(results.ParametersData[i].Estimates.Omega))
		}
		if len(results.ParametersData[i].Estimates.Sigma) != len(results.ParametersData[i].StdErr.Sigma) {
			results.ParametersData[i].StdErr.Sigma = make([]float64, len(results.ParametersData[i].Estimates.Sigma))
		}
	}

	if len(results.ShrinkageDetails.Eta.SD) == 0 {
		dim := 0
		for i := range results.ParameterStructures.Omega {
			if results.ParameterStructures.Omega[i] > 0 {
				dim++
			}
		}
		if dim > 0 {
			results.ShrinkageDetails.Eta.SD = make([]float64, dim)
			results.ShrinkageDetails.Eta.VR = make([]float64, dim)
			// Ebv follows Eta
			results.ShrinkageDetails.Ebv.SD = make([]float64, dim)
			results.ShrinkageDetails.Ebv.VR = make([]float64, dim)
		}
	}

	if len(results.ShrinkageDetails.Eps.SD) == 0 {
		dim := 0
		for i := range results.ParameterStructures.Sigma {
			if results.ParameterStructures.Sigma[i] > 0 {
				dim++
			}
		}
		if dim > 0 {
			results.ShrinkageDetails.Eps.SD = make([]float64, dim)
			results.ShrinkageDetails.Eps.VR = make([]float64, dim)
		}
	}

	return results
}
