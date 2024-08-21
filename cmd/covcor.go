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
	parser "github.com/metrumresearchgroup/bbi/parsers/nmparser"
	"github.com/metrumresearchgroup/bbi/utils"

	log "github.com/sirupsen/logrus"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const covcorExamples string = `  # Display .cov/cor values from run001/run001.{cov,cor}
  bbi nonmem covcor run001/run001
  # Display the same values by specifying a full output file
  bbi nonmem covcor run001/run001.cov`

func covcor(cmd *cobra.Command, args []string) {
	if debug {
		viper.Debug()
	}

	results, err := parser.GetCovCorOutput(args[0])
	if err != nil {
		log.Fatal(err)
	}

	err = utils.PrintJSON(results)
	if err != nil {
		log.Fatal(err)
	}
}

func NewCovcorCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "covcor [flags] <run file>",
		Short: "Display .cov and .cor output for a model",
		Long: `Read the .cov and .cor files from a model's output directory and
display the values as a JSON object. The argument is typically a shared prefix
for the run output files, but it can be any path from which the run name can be
derived.`,
		Example: covcorExamples,
		Run:     covcor,
	}
}
