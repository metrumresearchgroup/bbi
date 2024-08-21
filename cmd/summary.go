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
	"fmt"
	"os"
	"runtime"
	rdebug "runtime/debug"
	"sort"

	parser "github.com/metrumresearchgroup/bbi/parsers/nmparser"
	"github.com/metrumresearchgroup/bbi/utils"

	log "github.com/sirupsen/logrus"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var (
	noExt   bool
	noGrd   bool
	noShk   bool
	extFile string
)

const summaryExamples string = `  # Summarize run001
  bbi nonmem summary run001/run001.lst
  # The extension may be omitted
  bbi nonmem summary run001/run001
  # Output JSON summary for run001 and run002
  bbi nonmem summary --json run001/run001 run002/run002`

type jsonResults struct {
	Results []parser.SummaryOutput
	Errors  []int
}

func summarizeModel(model string) (sum parser.SummaryOutput, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("GetModelOutput: %v\n%v", r, string(rdebug.Stack()))
		}
	}()

	return parser.GetModelOutput(model, parser.NewModelOutputFile(extFile, noExt),
		!noGrd, !noShk)
}

func summary(_ *cobra.Command, args []string) {
	if debug {
		viper.Debug()
	}
	if len(args) == 1 {
		results, err := summarizeModel(args[0])
		if err == nil {
			results.Success = true
		} else {
			if Json {
				results = parser.SummaryOutput{ErrorMsg: err.Error()}
			} else {
				log.Fatal(err)
			}
		}
		if Json {
			err = utils.PrintJSON(results)
			if err != nil {
				log.Fatal(err)
			}
			if !results.Success {
				os.Exit(1)
			}
		} else {
			results.Summary()
		}

		return
	}

	type modelResult struct {
		Index  int
		Result parser.SummaryOutput
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

	var modelResults jsonResults
	modelResults.Results = make([]parser.SummaryOutput, numModels)
	modelResults.Errors = []int{}

	for w := 1; w <= workers; w++ {
		go func(modIndex <-chan int, results chan<- modelResult) {
			for i := range modIndex {
				r, err := summarizeModel(args[i])
				if err != nil {
					results <- modelResult{
						Index:  i,
						Result: parser.SummaryOutput{ErrorMsg: err.Error()},
					}
				} else {
					r.Success = true
					results <- modelResult{
						Index:  i,
						Result: r,
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

		modelResults.Results[res.Index] = res.Result
		if !res.Result.Success {
			modelResults.Errors = append(modelResults.Errors, res.Index)
		}
	}

	sort.Ints(modelResults.Errors)
	nerrors := len(modelResults.Errors)
	if Json {
		err := utils.PrintJSON(modelResults)
		if err != nil {
			log.Fatal(err)
		}
	} else {
		// not json lets print all successful models first then any errors
		nsuccessful := len(modelResults.Results) - nerrors
		for i, res := range modelResults.Results {
			if res.Success {
				res.Summary()
				// add some spacing between models
				if i != nsuccessful-1 {
					fmt.Println("")
					fmt.Println("")
				}
			}
		}
		for i := 0; i < nerrors; i++ {
			log.Error(modelResults.Results[modelResults.Errors[i]].ErrorMsg)
		}
	}
	if nerrors > 0 {
		os.Exit(1)
	}
}

func NewSummaryCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "summary [flags] <lst> [<lst>...]",
		Short: "Summarize model results",
		Long: `Summarize the results of the specified *.lst files. By default, this
prints a table of parameter estimates preceded by a lines with details about
the run. Pass the --json flag to get a machine-readable output that includes
more details.

The path may also be specified without the trailing ".lst". *.res files are
also supported.`,
		Example: summaryExamples,
		Run:     summary,
	}

	// Used for Summary
	cmd.PersistentFlags().BoolVar(&noExt, "no-ext-file", false, "do not use ext file")
	cmd.PersistentFlags().BoolVar(&noGrd, "no-grd-file", false, "do not use grd file")
	cmd.PersistentFlags().BoolVar(&noShk, "no-shk-file", false, "do not use shk file")
	cmd.PersistentFlags().StringVar(&extFile, "ext-file", "", "name of custom ext-file")

	return cmd
}
