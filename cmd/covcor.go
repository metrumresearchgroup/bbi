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

const covcorLongDescription string = `load .cov and .cor output from model(s), for example: 
bbi nonmem covcor run001/run001
bbi nonmem covcor run001/run001.cov
 `

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
		Use:   "covcor",
		Short: "load .cov and .cor output from a model run",
		Long:  covcorLongDescription,
		Run:   covcor,
	}
}
