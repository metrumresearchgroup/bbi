// Copyright © 2016 Devin Pastoor <devin.pastoor@gmail.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package cmd

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"runtime"
	"sort"
	"strconv"
	"strings"

	parser "github.com/metrumresearchgroup/bbi/parsers/nmparser"
	"github.com/metrumresearchgroup/bbi/utils"
	"github.com/scylladb/go-set/strset"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const paramsLongDescription string = `summarize model(s), for example:
bbi nonmem params run001
bbi nonmem params run001
bbi nonmem params run001
 `

var (
	noParamNames bool
	dir          string
)

// helpers for making a set.
func index(slice []string, item string) int {
	for i := range slice {
		if slice[i] == item {
			return i
		}
	}

	return -1
}

func printParamHeader(results parser.ExtFastData) {
	for i, s := range results.ParameterNames {
		results.ParameterNames[i] = strings.ReplaceAll(s, ",", "_")
	}
	fmt.Println("dir," + strings.Join(results.ParameterNames, ","))
}

func removeDuplicateValues(stringSlice []string) []string {
	keys := make(map[string]bool)
	list := []string{}

	for _, entry := range stringSlice {
		if _, value := keys[entry]; !value {
			keys[entry] = true
			list = append(list, entry)
		}
	}

	return list
}

// Parsed representation of a parameter name for sorting purposes.
type parsedParam struct {
	// raw parameter name (e.g., "OMEGA(1,2)")
	FullName string
	// name without string encoded index info (e.g., "OMEGA")
	BaseName string
	// position where BaseName first appears in a list of parameters
	Position int
	// parsed index information (e.g., `int[]{1,2}`)
	SubIndex []int
}

type parsedParams []parsedParam

func (xs parsedParams) Len() int {
	return len(xs)
}

func (xs parsedParams) Swap(i, j int) {
	xs[i], xs[j] = xs[j], xs[i]
}

func (xs parsedParams) Less(i, j int) bool {
	xi, xj := xs[i], xs[j]

	// If these aren't the same parameter, sort by their order in
	// the parameter original list.
	if xi.BaseName != xj.BaseName {
		return xi.Position < xj.Position
	}

	// Otherwise, if they have different shapes, sort by the
	// number of dimensions (fewer first).
	ndimI := len(xi.SubIndex)
	if ndimJ := len(xj.SubIndex); ndimI != ndimJ {
		return ndimI < ndimJ
	}

	// If the number of dimensions match, sort by the first index
	// value that differs, falling through to index values of the
	// last dimension.
	for i := 0; i < ndimI-1; i++ {
		if xi.SubIndex[i] != xj.SubIndex[i] {
			return xi.SubIndex[i] < xj.SubIndex[i]
		}
	}

	return xi.SubIndex[ndimI-1] < xj.SubIndex[ndimI-1]
}

// parseSubIndex converts a string representation of indices (e.g.,
// "(1,2,3)") into a list of ints (`[]int{1,2,3}`).
func parseSubIndex(s string) ([]int, error) {
	var idxs []int

	if len(s) > 2 && s[0] == '(' {
		strIdxs := strings.Split(s[1:len(s)-1], ",")
		idxs = make([]int, len(strIdxs))
		for i, strIdx := range strIdxs {
			if idx, err := strconv.Atoi(strIdx); err == nil {
				idxs[i] = idx
			} else {
				return nil, err
			}
		}
	} else {
		if idx, err := strconv.Atoi(s); err == nil {
			idxs = []int{idx}
		} else {
			return nil, err
		}
	}

	return idxs, nil
}

// splitParam splits a parameter that has encoded index information
// (e.g., "OMEGA(1,2)") into a base name and indices (e.g., "OMEGA"
// and `[]int{1,2}`).
//
// Note: This function is intended to be used only for sorting
// purposes, where the failure mode for a bad split is just unideal
// presentation.  As such, if parsing the index fails, this swallows
// the error and treats the full name as the parameter.
func splitParam(param string) (string, []int) {
	delims := []rune{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '('}
	sawNonDelim := false

	for i, pchar := range param {
		isDelim := false
		for _, dchar := range delims {
			if pchar == dchar {
				isDelim = true
				if sawNonDelim {
					s, err := parseSubIndex(param[i:])
					if err == nil {
						return param[:i], s
					}

					return param, nil
				}
			}
		}
		sawNonDelim = !isDelim
	}

	return param, nil
}

func parseParams(params []string) parsedParams {
	var pos int

	parsed := make([]parsedParam, len(params))
	baseIdxs := make(map[string]int)

	for paramIdx, param := range params {
		base, subindex := splitParam(param)
		if baseIdx, known := baseIdxs[base]; known {
			pos = baseIdx
		} else {
			pos = paramIdx
			baseIdxs[base] = paramIdx
		}

		parsed[paramIdx] = parsedParam{
			FullName: param,
			BaseName: base,
			Position: pos,
			SubIndex: subindex,
		}
	}

	return parsed
}

func sortParams(params []string) []string {
	parsed := parseParams(params)
	sort.Stable(parsed)

	sorted := make([]string, len(params))
	for i, p := range parsed {
		sorted[i] = p.FullName
	}

	return sorted
}

func params(cmd *cobra.Command, args []string) {
	if debug {
		viper.Debug()
	}
	var modelDirs []string
	if dir != "" {
		fi, err := ioutil.ReadDir(dir)
		if err != nil {
			log.Fatal(err)
		}
		potentialModelDirs := utils.ListDirNames(fi)
		ctlModels := utils.ListFilesByExt(utils.ListFiles(fi), "ctl")
		modModels := utils.ListFilesByExt(utils.ListFiles(fi), "mod")
		md := append(ctlModels, modModels...)

		modelsInDir := strset.New()
		for _, f := range md {
			fileName, _ := utils.FileAndExt(f)
			modelsInDir.Add(fileName)
		}

		for _, pd := range potentialModelDirs {
			if modelsInDir.Has(pd) {
				modelDirs = append(modelDirs, pd)
			}
		}

		if len(modelDirs) == 0 {
			log.Infof("found %d subdirectories", len(potentialModelDirs))
			log.Infof("found %d models", len(modelsInDir.List()))
			log.Fatal("no subdirectories with corresponding ctl or mod files found")
		}
	}

	if len(args) == 1 {
		moddir := args[0]
		if extFile == "" {
			extFile = strings.Join([]string{filepath.Base(moddir), "ext"}, ".")
		}
		results, err := parser.ParseEstimatesFromExt(filepath.Join(moddir, extFile))

		if err != nil {
			log.Fatal(err)
		}
		if Json {
			jsonRes, err := json.MarshalIndent(results, "", "\t")
			if err != nil {
				log.Fatal(err)
			}
			fmt.Printf("%s\n", jsonRes)
		} else {
			if !noParamNames {
				printParamHeader(results)
			}

			fmt.Println(extFile + "," + strings.Join(results.EstimationLines[len(results.EstimationLines)-1], ","))
		}

		return
	}

	// if we are going to parse multiple models, we need to reasonably handle failures. The objective
	// will be to always return a json object if its json, and if not, error as soon as it hits a printed issue.
	// As such, the idea will be to store results such they can be filtered
	type result int
	const (
		SUCCESS result = 1
		ERROR   result = 2
	)
	type paramResult struct {
		Index   int
		Outcome result
		Err     error
		Result  parser.ExtFastData
	}
	type jsonParamResults struct {
		Results []parser.ExtFastData
		Errors  []error
	}

	workers := runtime.NumCPU()
	if workers < 4 {
		workers = 4
	}

	numModels := len(modelDirs)
	if workers > numModels {
		workers = numModels
	}
	models := make(chan int, numModels)
	results := make(chan paramResult, numModels)
	orderedResults := make([]paramResult, numModels)
	var paramResults jsonParamResults

	for w := 1; w <= workers; w++ {
		go func(modIndex <-chan int, results chan<- paramResult) {
			for i := range modIndex {
				moddir := filepath.Join(dir, modelDirs[i])
				var extFileName string
				if extFile != "" {
					extFileName = extFile
				} else {
					extFileName = strings.Join([]string{filepath.Base(moddir), "ext"}, ".")
				}
				r, err := parser.ParseEstimatesFromExt(filepath.Join(moddir, extFileName))

				if err != nil {
					results <- paramResult{
						Index:   i,
						Outcome: ERROR,
						Err:     err,
						Result:  r,
					}
				} else {
					results <- paramResult{
						Index:   i,
						Outcome: SUCCESS,
						Err:     err,
						Result:  r,
					}
				}
			}
		}(models, results)
	}

	for m := 0; m < numModels; m++ {
		models <- m
	}
	close(models)
	for r := 0; r < numModels; r++ {
		res := <-results
		orderedResults[res.Index] = res
	}

	if !Json {
		paramSet := []string{}
		for _, res := range orderedResults {
			results := res.Result
			paramSet = append(paramSet, results.ParameterNames...)
		}

		paramSet = sortParams(removeDuplicateValues(paramSet))
		for i, s := range paramSet {
			paramSet[i] = strings.ReplaceAll(s, ",", "_")
		}

		fmt.Println("dir,error,termination," + strings.Join(paramSet, ","))
		for _, res := range orderedResults {
			s := make([]string, len(paramSet))
			for i := range s {
				s[i] = ""
			}

			var absolutePath string
			if strings.HasSuffix(dir, "/") {
				absolutePath = dir + modelDirs[res.Index]
			} else {
				absolutePath = dir + "/" + modelDirs[res.Index]
			}

			if res.Outcome == SUCCESS {
				results := res.Result
				for i, name := range results.ParameterNames {
					idx := index(paramSet, strings.ReplaceAll(name, ",", "_"))
					values := results.EstimationLines
					s[idx] = values[len(values)-1][i]
				}

				// first code is the termination status
				terminationCode := results.TerminationCodes[0][0]
				fmt.Println(absolutePath + ",," + terminationCode + "," + strings.Join(s, ","))
			} else if res.Outcome == ERROR {
				errorMessage := res.Err.Error()
				fmt.Println(absolutePath + "," + errorMessage + ",," + strings.Join(s, ","))
			}
		}

		return
	}

	for _, res := range orderedResults {
		if res.Outcome == SUCCESS {
			paramResults.Results = append(paramResults.Results, res.Result)
		} else if res.Outcome == ERROR {
			paramResults.Errors = append(paramResults.Errors, res.Err)
		}
	}

	jsonRes, err := json.MarshalIndent(paramResults, "", "\t")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%s\n", jsonRes)
}

func NewParamsCmd() *cobra.Command {
	var cmd = &cobra.Command{
		Use:   "params",
		Short: "get the parameters of model(s)",
		Long:  paramsLongDescription,
		Run:   params,
	}

	//Used for Summary
	cmd.PersistentFlags().StringVar(&extFile, "ext-file", "", "name of custom ext-file")
	cmd.PersistentFlags().StringVar(&dir, "dir", "", "name of directory to look for runs")
	cmd.PersistentFlags().BoolVar(&noParamNames, "no-names", false, "don't print a header of names")

	return cmd
}
