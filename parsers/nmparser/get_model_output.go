package parser

import (
	"errors"
	"fmt"
	"github.com/thoas/go-funk"
	"math"
	"os"
	"path/filepath"
	"strconv"
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
func GetModelOutput(lstPath string, ext ModelOutputFile, grd bool, cov bool, cor bool, shk bool) ModelOutput {

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
	}, results.RunDetails.EstimationMethods[len(results.RunDetails.EstimationMethods)-1])

	cpuFilePath := filepath.Join(dir, runNum+".cpu")
	cpuLines, err := utils.ReadLines(cpuFilePath)
	if err != nil {
		// this is set to trace as don't want it to log normally as could screw up json output that
		// requests results from this such as summary --json
		log.Trace("error reading cpu file: %v", err)
	} else {
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(cpuFilePath))
	}
	cpuTime, err := strconv.ParseFloat(strings.TrimSpace(cpuLines[0]), 64)
	if err != nil {
		// this is set to trace as don't want it to log normally as could screw up json output that
		log.Trace("error parsing cpu time: %v", err)
		cpuTime = math.Inf(-1)
	}
	results.RunDetails.CpuTime = cpuTime

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

	if grd && !isNotGradientBased {
		name := runNum + ".grd"
		grdFilePath := filepath.Join(dir, name)
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

	if cov {
		name := runNum + ".cov"
		covFilePath := filepath.Join(dir, name)
		errorIfNotExists(AppFs, covFilePath, "--no-cov-file")
		covLines, err := utils.ReadLines(covFilePath)
		if err != nil {
			log.Fatalf("error reading outputs from cov file: %s \n", err)
		}
		results.CovarianceTheta = GetThetaValues(covLines)
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(covFilePath))
	}

	if cor {
		name := runNum + ".cor"
		corFilePath := filepath.Join(dir, name)
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
	if shk {
		name := runNum + ".shk"
		shkFilePath := filepath.Join(dir, name)
		errorIfNotExists(AppFs, shkFilePath, "--no-shk-file")
		shkLines, err := utils.ReadLines(shkFilePath)
		if err != nil {
			if errors.Is(err, os.ErrNotExist) {
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

func errorIfNotExists(fs afero.Fs, path string, sFlag string) {
	exists, err := utils.Exists(path, fs)
	if err != nil {
		panic(fmt.Sprintf("unknown error checking file existence %s\n", err))
	}
	if !exists {
		suppressionFlagMsg := "\n"
		if sFlag != "" {
			suppressionFlagMsg = fmt.Sprintf("\nyou can suppress bbi searching for the file using %s\n", sFlag)
		}
		log.Fatalf("No file present at %s%s ", path, suppressionFlagMsg)
	}
	return
}
