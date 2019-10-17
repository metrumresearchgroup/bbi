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
func GetModelOutput(filePath string, verbose bool, noExt bool, noGrd bool, noCov bool, noCor bool) ModelOutput {

	AppFs := afero.NewOsFs()
	runNum, _ := utils.FileAndExt(filePath)
	dir, _ := filepath.Abs(filepath.Dir(filePath))
	outputFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".lst"}, "")

	if verbose {
		log.Printf("base dir: %s", dir)
	}

	fileLines, _ := utils.ReadLinesFS(AppFs, outputFilePath)
	results := ParseLstEstimationFile(fileLines)
	results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(outputFilePath))

	if !noExt {
		extFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".ext"}, "")
		extLines, err := utils.ReadParamsAndOutputFromExt(extFilePath)
		if err != nil {
			panic(err)
		}
		extData, _ := ParseExtData(ParseExtLines(extLines))
		results.ParametersData = extData
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(extFilePath))
	}

	if !noGrd {
		grdFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".grd"}, "")
		grdLines, err := utils.ReadParamsAndOutputFromExt(grdFilePath)
		if err != nil {
			panic(err)
		}
		parametersData, _ := ParseGrdData(ParseGrdLines(grdLines))
		results.RunHeuristics.HasFinalZeroGradient = HasZeroGradient(parametersData[len(parametersData)-1].Fixed.Theta)
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(grdFilePath))
	}

	if !noCov {
		covFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".cov"}, "")
		covLines, err := utils.ReadLines(covFilePath)
		if err == nil {
			results.CovarianceTheta = GetThetaValues(covLines)
			results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(covFilePath))
		}
	}

	if !noCor {
		corFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".cor"}, "")
		corLines, err := utils.ReadLines(corFilePath)
		if err == nil {
			results.CorrelationTheta = GetThetaValues(corLines)
			results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(corFilePath))
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

	for n := 0; n < len(results.ShrinkageDetails); n++ {
		if len(results.ShrinkageDetails[n].EtaSD) == 0 {
			dim := 0
			for i := range results.ParameterStructures.Omega {
				if results.ParameterStructures.Omega[i] > 0 {
					dim++
				}
			}
			if dim > 0 {
				results.ShrinkageDetails[n].EtaSD = make([]float64, dim)
				results.ShrinkageDetails[n].EtaVR = make([]float64, dim)
				// Ebv follows Eta
				results.ShrinkageDetails[n].EbvSD = make([]float64, dim)
				results.ShrinkageDetails[n].EbvVR = make([]float64, dim)
			}
		}

		if len(results.ShrinkageDetails[n].EpsSD) == 0 {
			dim := 0
			for i := range results.ParameterStructures.Sigma {
				if results.ParameterStructures.Sigma[i] > 0 {
					dim++
				}
			}
			if dim > 0 {
				results.ShrinkageDetails[n].EpsSD = make([]float64, dim)
				results.ShrinkageDetails[n].EpsVR = make([]float64, dim)
			}
		}
	}

	return results
}
