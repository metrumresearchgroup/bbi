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
func GetModelOutput(filePath string, verbose bool) ModelOutput {

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

	extFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".ext"}, "")
	extLines, err := utils.ReadParamsAndOutputFromExt(extFilePath)
	if err == nil {
		extData, _ := ParseExtData(ParseExtLines(extLines))
		results.ParametersData = extData
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, extFilePath)
	}

	grdFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".grd"}, "")
	grdLines, err := utils.ReadParamsAndOutputFromExt(grdFilePath)
	if err == nil {
		parametersData, _ := ParseGrdData(ParseGrdLines(grdLines))
		results.RunHeuristics.HasFinalZeroGradient = HasZeroGradient(parametersData[len(parametersData)-1].Fixed.Theta).String()
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, grdFilePath)
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
	return results
}
