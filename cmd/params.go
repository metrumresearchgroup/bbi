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
	parser "github.com/metrumresearchgroup/bbi/parsers/nmparser"
	"github.com/metrumresearchgroup/bbi/utils"
	"github.com/scylladb/go-set/strset"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"io/ioutil"
	"path/filepath"
	"runtime"
	"strings"
)

const paramsLongDescription string = `summarize model(s), for example: 
bbi nonmem params run001
bbi nonmem params run001
bbi nonmem params run001
 `

var (
	noParamNames bool
	dir string
)

// helpers for making a set
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
		md := utils.ListFilesByExt(utils.ListFiles(fi), "ctl")
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
			log.Infof("found %s subdirectories", len(potentialModelDirs))
			log.Infof("found %s models", len(modelsInDir.List()))
			log.Fatal("no subdirectories with corresponding ctl files found")
		}
	}

	if len(args) == 1 {
		dir := args[0]
		if extFile == "" {
			extFile = strings.Join([]string{filepath.Base(dir), "ext"},".")
		}
		results, err := parser.ParseEstimatesFromExt(filepath.Join(dir, extFile))

		if err != nil {
			log.Fatal(err)
		}
		if Json {
			jsonRes, _ := json.MarshalIndent(results, "", "\t")
			fmt.Printf("%s\n", jsonRes)
		} else {
			if !noParamNames {
				printParamHeader(results)
			}

			fmt.Println(extFile + "," + strings.Join(results.EstimationLines[len(results.EstimationLines) - 1], ","))
		}
		return
	}

	// if we are going to parse multiple models, we need to reasonably handle failures. The objective
	// will be to always return a json object if its json, and if not, error as soon as it hits a printed issue.
	// As such, the idea will be to store results such they can be filtered
	type result int
	const (
		SUCCESS result = 1
		ERROR          = 2
	)
	type paramResult struct {
		Index   int
		Outcome result
		Err     error
		Result  parser.ExtFastData
	}
	type jsonParamResults struct {
		Results []parser.ExtFastData
		Errors []error
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
		go func(w int, modIndex <-chan int, results chan<- paramResult) {
			for i := range modIndex {
				dir := filepath.Join(dir, modelDirs[i])
				var extFileName string
				if extFile != "" {
					extFileName = extFile
				} else {
					extFileName = strings.Join([]string{filepath.Base(dir), "ext"}, ".")
				}
				r, err := parser.ParseEstimatesFromExt(filepath.Join(dir, extFileName))

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
		}(w, models, results)
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

		paramSet = removeDuplicateValues(paramSet)
		for i, s := range paramSet {
			paramSet[i] = strings.ReplaceAll(s, ",", "_")
		}

		fmt.Println("dir,error,termination," + strings.Join(paramSet, ","))
		for _, res := range orderedResults {
			s := make([]string, len(paramSet))
			for i, _ := range s {
				s[i] = ""
			}

			absolutePath := dir + "/" + modelDirs[res.Index]
			if res.Outcome == SUCCESS {
				results := res.Result
				for i, name := range results.ParameterNames {
					idx := index(paramSet, strings.ReplaceAll(name, ",", "_"))
					values := results.EstimationLines
					s[idx] = values[0][i]
				}

				// first code is the termination status
				terminationCode := results.TerminationCodes[0][0]
				fmt.Println(absolutePath + ",," + terminationCode + "," + strings.Join(s, ","))

			} else if res.Outcome == ERROR {
				errorMessage :=  res.Err.Error()
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

	jsonRes, _ := json.MarshalIndent(paramResults, "", "\t")
	fmt.Printf("%s\n", jsonRes)
	return
}

func NewParamsCmd() *cobra.Command {
	var cmd = &cobra.Command{
		Use:   "params",
		Short: "get the parameters of model(s)",
		Long: paramsLongDescription,
		Run: params,
	}

	//Used for Summary
	cmd.PersistentFlags().StringVar(&extFile, "ext-file", "", "name of custom ext-file")
	cmd.PersistentFlags().StringVar(&dir, "dir", "", "name of directory to look for runs")
	cmd.PersistentFlags().BoolVar(&noParamNames, "no-names", false, "don't print a header of names")

	return cmd
}
