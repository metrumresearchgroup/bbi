package parser

import (
	"fmt"
	"github.com/thoas/go-funk"
	"os"
	"path/filepath"
	"strings"

	"github.com/metrumresearchgroup/babylon/utils"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/afero"
)


// ModelOutputFile gives the name of the summary file and whether to include the data
// in the model output
type ModelOutputFile struct {
	Exclude bool
	Name    string
}


// NewModelOutputFile returns a ModelOutputFile with a name and exclusion
// given no name is set, downstream code should expect standard naming conventions following root.extension syntax
// for example given a model 100 and want to set the ext file
// if ModelOutputFile.Name is "" should look for 100.ext
func NewModelOutputFile(name string, exclude bool) ModelOutputFile {
	return ModelOutputFile{Name: name, Exclude: exclude}
}
// GetModelOutput populates and returns a ModelOutput object by parsing files
// if ext file is excluded, will attempt to parse the lst file for additional information traditionally available there
func GetModelOutput(lstPath string, ext, grd, cov, cor, shk ModelOutputFile) ModelOutput {

	AppFs := afero.NewOsFs()
	runNum, _ := utils.FileAndExt(lstPath)
	dir, _ := filepath.Abs(filepath.Dir(lstPath))
	outputFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".lst"}, "")

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


	if !ext.Exclude {
		if ext.Name == "" {
			ext.Name = runNum + ".ext"
		}
		extFilePath := filepath.Join(dir, ext.Name)
	    errorIfNotExists(AppFs, extFilePath, "--no-ext-file")
		extLines, err := utils.ReadParamsAndOutputFromExt(extFilePath)
		if err != nil {
			log.Fatalf("error reading outputs from ext file: %s \n", err)
		}
		extData, parameterNames := ParseExtData(ParseExtLines(extLines))
		results.ParametersData = extData
		results.ParameterNames.Omega = parameterNames.Omega
		results.ParameterNames.Sigma = parameterNames.Sigma

		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(extFilePath))
	}

	if !grd.Exclude && !isNotGradientBased {
		if grd.Name == "" {
			grd.Name = runNum + ".grd"
		}
		grdFilePath := filepath.Join(dir, ext.Name)
		errorIfNotExists(AppFs, grdFilePath, "--no-grd-file")
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

	if !cov.Exclude {
		if cov.Name == "" {
			cov.Name = runNum + ".cov"
		}
		covFilePath := filepath.Join(dir, cov.Name)
		errorIfNotExists(AppFs, covFilePath, "--no-cov-file")
		covLines, err := utils.ReadLines(covFilePath)
		if err != nil {
			log.Fatalf("error reading outputs from cov file: %s \n", err)
		}
		results.CovarianceTheta = GetThetaValues(covLines)
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(covFilePath))
	}

	if !cor.Exclude {
		if cor.Name == "" {
			cor.Name = runNum + ".cor"
		}
		corFilePath := filepath.Join(dir, cor.Name)
		errorIfNotExists(AppFs, corFilePath, "--no-cor-file")
		corLines, err := utils.ReadLines(corFilePath)
		if err != nil {
			log.Fatalf("error reading outputs from cov file: %s \n", err)
		}
		results.CorrelationTheta = GetThetaValues(corLines)
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(corFilePath))
	}

	etaCount := lowerDiagonalLengthToDimension[len(results.ParametersData[len(results.ParametersData)-1].Estimates.Omega)]
	epsCount := lowerDiagonalLengthToDimension[len(results.ParametersData[len(results.ParametersData)-1].Estimates.Sigma)]
	// bayesian model runs will never have shrinkage files
	if !shk.Exclude {
		if shk.Name == "" {
			shk.Name = runNum + ".shk"
		}
		shkFilePath := filepath.Join(dir, shk.Name)
		errorIfNotExists(AppFs, shkFilePath, "--no-shk-file")
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

func errorIfNotExists (fs afero.Fs, path string, sFlag string) {
	exists, err := utils.Exists(path, fs)
	if err != nil {
		panic(fmt.Sprintf("unknown error checking file existence %s\n", err))
	}
	if !exists {
		suppressionFlagMsg := "\n"
		if sFlag != "" {
			suppressionFlagMsg = fmt.Sprintf( "\nyou can suppress bbi searching for the file using %s\n", sFlag)
		}
		log.Fatalf("No file present at %s%s ", path, suppressionFlagMsg)
	}
	return
}