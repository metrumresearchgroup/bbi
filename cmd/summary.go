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

	parser "github.com/metrumresearchgroup/babylon/parsers/nmparser"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var summaryTree bool

// runCmd represents the run command
var summaryCmd = &cobra.Command{
	Use:   "summary",
	Short: "summarize the output of a model",
	Long: `run model(s), for example: 
nmu summarize run001.lst
 `,
	Run: summary,
}

func summary(cmd *cobra.Command, args []string) {
	if debug {
		viper.Debug()
	}

	results := parser.GetModelOutput(args[0], verbose)

	if Json {
		jsonRes, _ := json.MarshalIndent(results, "", "\t")
		fmt.Printf("%s\n", jsonRes)
	} else {
		results.Summary()
	}

}
func init() {
	RootCmd.AddCommand(summaryCmd)
}
