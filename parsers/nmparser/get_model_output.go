package parser

import (
	"errors"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/metrumresearchgroup/bbi/utils"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/afero"
)

// ModelOutputFile gives the name of the summary file and whether to include the data
// in the model output.
type ModelOutputFile struct {
	Exclude bool
	Name    string
}

// NewModelOutputFile returns a ModelOutputFile with a name and exclusion
// given no name is set, downstream code should expect standard naming conventions following root.extension syntax
// for example given a model 100 and want to set the ext file
// if ModelOutputFile.Name is "" should look for 100.ext.
func NewModelOutputFile(name string, exclude bool) ModelOutputFile {
	return ModelOutputFile{Name: name, Exclude: exclude}
}

// GetModelOutput populates and returns a SummaryOutput object by parsing files
// if ext file is excluded, will attempt to parse the lst file for additional information traditionally available there.
func GetModelOutput(lstPath string, ext ModelOutputFile, grd bool, shk bool) (SummaryOutput, error) {
	AppFs := afero.NewOsFs()
	runNum, extension := utils.FileAndExt(lstPath)
	if extension == "" {
		extension = ".lst"
	}
	// though lst is vastly more used, some examples from ICON use .res
	if extension != ".lst" && extension != ".res" {
		err_msg := fmt.Sprintf("Must provide path to .lst (or .res) file for summary but provided '%s'", lstPath)
		err_msg += "\nCan also pass no extension and summary will infer .lst extension."

		return SummaryOutput{}, errors.New(err_msg)
	}

	dir, _ := filepath.Abs(filepath.Dir(lstPath))
	outputFilePath := strings.Join([]string{filepath.Join(dir, runNum), extension}, "")

	fileLines, err := utils.ReadLinesFS(AppFs, outputFilePath)
	if err != nil {
		return SummaryOutput{}, err
	}
	results := ParseLstEstimationFile(fileLines)
	if results.RunDetails.DataSet == DefaultString {
		return results, fmt.Errorf("no DATA line found in %v", lstPath)
	}
	if results.RunDetails.RunEnd == DefaultString {
		return results, fmt.Errorf(
			"%v does not contain 'Stop time:' or 'Finished'. Incomplete run?",
			lstPath)
	}

	results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(outputFilePath))

	var readGrd bool
	var readShk bool

	if results.RunDetails.OnlySim {
		if len(results.RunDetails.EstimationMethods) > 0 {
			msg := "no estimation methods expected for ONLYSIM"

			return SummaryOutput{}, errors.New(msg)
		}

		readGrd = false
		readShk = false
	} else {
		// if the final method is one of these, don't look for .grd file
		readGrd = grd && !CheckIfNotGradientBased(results)
		// if the final method is one of these, don't look for .shk file
		readShk = shk && !CheckIfBayesian(results)
	}

	cpuFilePath := filepath.Join(dir, runNum+".cpu")
	cpuLines, err := utils.ReadLines(cpuFilePath)
	if err != nil {
		// this is set to trace as don't want it to log normally as could screw up json output that
		// requests results from this such as summary --json
		log.Trace("error reading cpu file: ", err)
	} else {
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(cpuFilePath))
		cpuTime, err := parseFloatReplaceNaN(strings.TrimSpace(cpuLines[0]))
		if err != nil {
			// this is set to trace as don't want it to log normally as could screw up json output that
			log.Trace("error parsing cpu time: ", err)
			results.RunDetails.CpuTime = DefaultFloat64
		}
		results.RunDetails.CpuTime = cpuTime
	}

	if !ext.Exclude && !results.RunDetails.OnlySim {
		if ext.Name == "" {
			ext.Name = runNum + ".ext"
		}
		extFilePath := filepath.Join(dir, ext.Name)
		err := errorIfNotExists(AppFs, extFilePath, "--no-ext-file")
		if err != nil {
			return SummaryOutput{}, err
		}
		extLines, err := utils.ReadParamsAndOutputFromExt(extFilePath)
		if err != nil {
			return SummaryOutput{}, err
		}
		extLinesParsed := ParseExtLines(extLines)

		// Parse parameters from .ext
		extData, parameterNames := ParseParamsExt(extLinesParsed)
		results.ParametersData = extData
		results.ParameterNames.Omega = parameterNames.Omega
		results.ParameterNames.Sigma = parameterNames.Sigma

		// Parse condition number
		results.ConditionNumber = ParseConditionNumberExt(extLinesParsed)
		results.RunHeuristics.LargeConditionNumber = getLargeConditionNumber(results.ConditionNumber)

		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(extFilePath))
	}

	if readGrd {
		name := runNum + ".grd"
		grdFilePath := filepath.Join(dir, name)
		err := errorIfNotExists(AppFs, grdFilePath, "--no-grd-file")
		if err != nil {
			return SummaryOutput{}, err
		}
		grdLines, err := utils.ReadLinesFS(AppFs, grdFilePath)
		if err != nil {
			return SummaryOutput{}, err
		}
		parametersData, _ := ParseGrdData(ParseGrdLines(grdLines))
		results.RunHeuristics.HasFinalZeroGradient = utils.HasZero(parametersData[len(parametersData)-1].Fixed.Theta)
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(grdFilePath))
	}

	etaCount, _ := lowerDiagonalLengthToDimension(len(results.ParametersData[len(results.ParametersData)-1].Estimates.Omega))
	epsCount, _ := lowerDiagonalLengthToDimension(len(results.ParametersData[len(results.ParametersData)-1].Estimates.Sigma))
	// bayesian model runs will never have shrinkage files
	if readShk {
		name := runNum + ".shk"
		shkFilePath := filepath.Join(dir, name)
		err := errorIfNotExists(AppFs, shkFilePath, "--no-shk-file")
		if err != nil {
			return SummaryOutput{}, err
		}
		shkLines, err := utils.ReadLines(shkFilePath)
		if err != nil {
			return SummaryOutput{}, err
		}
		results.ShrinkageDetails = ParseShkData(ParseShkLines(shkLines), etaCount, epsCount)
		results.RunDetails.OutputFilesUsed = append(results.RunDetails.OutputFilesUsed, filepath.Base(shkFilePath))

		// check Eta Pval heuristic
		// note, if multiple subpops then there is no p-value test
		if len(results.ShrinkageDetails[len(results.ShrinkageDetails)-1]) == 1 {
			finalShrinkage := results.ShrinkageDetails[len(results.ShrinkageDetails)-1][0]
			b := make([]bool, len(finalShrinkage.Pval))
			for i, n := range finalShrinkage.Pval {
				b[i] = n < 0.05 && n > 0.0
			}
			results.RunHeuristics.EtaPvalSignificant = utils.AnyTrue(b)
		}
	}

	// Extra heuristics
	results.RunHeuristics.PRDERR, _ = utils.Exists(filepath.Join(dir, "PRDERR"), AppFs)

	setMissingValuesToDefault(&results)

	return results, nil
}

func errorIfNotExists(fs afero.Fs, path string, sFlag string) error {
	exists, err := utils.Exists(path, fs)
	if err != nil {
		panic(fmt.Sprintf("unknown error checking file existence: %s\n", err.Error()))
	}
	if !exists {
		suppressionFlagMsg := "\n"
		if sFlag != "" {
			suppressionFlagMsg = fmt.Sprintf("\nyou can suppress bbi searching for the file using %s\n", sFlag)
		}

		return fmt.Errorf("no file present at %s %s", path, suppressionFlagMsg)
	}

	return nil
}

// GetCovCorOutput
// STILL UNDER CONSTRUCTION.
func GetCovCorOutput(lstPath string) (CovCorOutput, error) {
	AppFs := afero.NewOsFs()
	runNum, _ := utils.FileAndExt(lstPath)
	dir, _ := filepath.Abs(filepath.Dir(lstPath))

	covFilePath := filepath.Join(dir, runNum+".cov")
	err := errorIfNotExists(AppFs, covFilePath, "")
	if err != nil {
		return CovCorOutput{}, err
	}
	covLines, err := utils.ReadLines(covFilePath)
	if err != nil {
		return CovCorOutput{}, err
	}
	covarianceTheta := GetThetaValues(covLines)

	corFilePath := filepath.Join(dir, runNum+".cor")
	err = errorIfNotExists(AppFs, corFilePath, "")
	if err != nil {
		return CovCorOutput{}, err
	}
	corLines, err := utils.ReadLines(corFilePath)
	if err != nil {
		return CovCorOutput{}, err
	}
	correlationTheta := GetThetaValues(corLines)

	results := CovCorOutput{
		CovarianceTheta:  covarianceTheta,
		CorrelationTheta: correlationTheta,
	}

	return results, nil
}
