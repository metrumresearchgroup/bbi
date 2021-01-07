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
	log "github.com/sirupsen/logrus"
	"os"
	"runtime"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var (
	summaryTree bool
	noExt       bool
	noGrd       bool
	noShk       bool
	extFile     string
)
const summaryLongDescription string = `summarize model(s), for example: 
bbi nonmem summary run001/run001
bbi nonmem summary run001/run001.lst
bbi nonmem summary run001/run001.res
 `

// runCmd represents the run command
var summaryCmd = &cobra.Command{
	Use:   "summary",
	Short: "summarize the output of model(s)",
	Long: summaryLongDescription,
	Run: summary,
}

type jsonResults struct {
	Results []parser.SummaryOutput
	Errors  []error
}

func summary(cmd *cobra.Command, args []string) {
	if debug {
		viper.Debug()
	}
	if len(args) == 1 {
		results, err := parser.GetModelOutput(args[0], parser.NewModelOutputFile(extFile, noExt), !noGrd, !noShk, )
		if err != nil {
			log.Fatal(err)
		}
		if Json {
			jsonRes, _ := json.MarshalIndent(results, "", "\t")
			fmt.Printf("%s\n", jsonRes)
		} else {
			results.Summary()
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
	type modelResult struct {
		Index   int
		Outcome result
		Err     error
		Result  parser.SummaryOutput
	}

	workers := runtime.NumCPU()
	if workers < 4 {
		workers = 4
	}

	numModels := len(args)
	if workers > numModels {
		workers = numModels
	}
	models := make(chan int, numModels)
	results := make(chan modelResult, numModels)
	orderedResults := make([]modelResult, numModels)
	var modelResults jsonResults

	for w := 1; w <= workers; w++ {
		go func(w int, modIndex <-chan int, results chan<- modelResult) {
			for i := range modIndex {
				r, err := parser.GetModelOutput(args[i], parser.NewModelOutputFile(extFile, noExt), !noGrd, !noShk, )
				if err != nil {
					results <- modelResult{
						Index:   i,
						Outcome: ERROR,
						Err:     err,
						Result:  r,
					}
				} else {
					results <- modelResult{
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
	for _, res := range orderedResults {
		if res.Outcome == SUCCESS {
			modelResults.Results = append(modelResults.Results, res.Result)
		} else if res.Outcome == ERROR {
			modelResults.Errors = append(modelResults.Errors, res.Err)
		}
	}
	if Json {
		jsonRes, _ := json.MarshalIndent(modelResults, "", "\t")
		fmt.Printf("%s\n", jsonRes)
		return
	}


	// not json lets print all successful models first then any errors
	for i, res := range modelResults.Results {
		res.Summary()
		// add some spacing between models
		if i != len(modelResults.Results) -1 {
			fmt.Println("")
			fmt.Println("")
		}
	}
	for _, res := range modelResults.Errors {
		log.Error(res)
	}
	if len(modelResults.Errors) > 0 {
		os.Exit(1)
	}
}
func init() {
	nonmemCmd.AddCommand(summaryCmd)
	//Used for Summary
	summaryCmd.PersistentFlags().BoolVar(&noExt, "no-ext-file", false, "do not use ext file")
	summaryCmd.PersistentFlags().BoolVar(&noGrd, "no-grd-file", false, "do not use grd file")
	summaryCmd.PersistentFlags().BoolVar(&noShk, "no-shk-file", false, "do not use shk file")
	summaryCmd.PersistentFlags().StringVar(&extFile, "ext-file", "", "name of custom ext-file")
}
