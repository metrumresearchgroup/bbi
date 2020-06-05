package parser

import (
	"github.com/thoas/go-funk"
	"os"
	"path/filepath"
	"strings"

	"github.com/metrumresearchgroup/babylon/utils"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/afero"
)

// GetModelOutput populates and returns a ModelOutput object by parsing files
// ParameterData is parsed from the ext file when useExtFile is true
// ParameterData is parsed from the lst file when useExtFile is false
func GetModelOutput(filePath string, verbose, noExt, noGrd, noCov, noCor, noShk bool) ModelOutput {

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
	// if bayesian, not aware of any times people ever do a prelim estimation with a different method
	isNotGradientBased := funk.Contains([]string{"MCMC Bayesian Analysis",
		"Stochastic Approximation Expectation-Maximization",
		"Importance Sampling assisted by MAP Estimation",
		"Importance Sampling",
		"NUTS Bayesian Analysis",
		"Objective Function Evaluation by Importance Sampling",
	} , results.RunDetails.EstimationMethods[len(results.RunDetails.EstimationMethods) - 1])


	if !noExt {
		extFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".ext"}, "")
		extLines, err := utils.ReadParamsAndOutputFromExt(extFilePath)
		if err != nil {
			panic(err)
		}
		extData, parameterNames := ParseExtData(ParseExtLines(extLines))
		results.ParametersData = extData
		results.ParameterNames.Omega = parameterNames.Omega
		results.ParameterNames.Sigma = parameterNames.Sigma

		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(extFilePath))
	}

	if !noGrd && !isNotGradientBased {
		grdFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".grd"}, "")
		grdLines, err := utils.ReadLinesFS(AppFs, grdFilePath)
		if err != nil {
			if os.IsNotExist(err) {
				log.Error("no gradient file exists at: " + grdFilePath)
			} else {
				panic(err)
			}
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
	etaCount := lowerDiagonalLengthToDimension[len(results.ParametersData[len(results.ParametersData)-1].Estimates.Omega)]
	epsCount := lowerDiagonalLengthToDimension[len(results.ParametersData[len(results.ParametersData)-1].Estimates.Sigma)]
	// bayesian model runs will never have shrinkage files
	if !noShk && !isNotGradientBased {
		shkFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".shk"}, "")
		shkLines, err := utils.ReadLines(shkFilePath)
		if err != nil {
			if os.IsNotExist(err) {
				log.Error("no shrinkage file exists at: " + shkFilePath)
			} else {
				panic(err)
			}
		}
		results.ShrinkageDetails = ParseShkData(ParseShkLines(shkLines), etaCount, epsCount)
	}

	setMissingValuesToDefault(&results, etaCount, epsCount)
	return results
}
