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
	"log"
	"path/filepath"
	"strings"

	parser "github.com/dpastoor/babylon/parsers/nmparser"
	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
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

	AppFs := afero.NewOsFs()

	filePath := args[0]

	// create a new dir for model estimation
	runNum, _ := utils.FileAndExt(filePath)
	dir, _ := filepath.Abs(filepath.Dir(filePath))
	outputFilePath := strings.Join([]string{filepath.Join(dir, runNum), ".lst"}, "")
	if verbose {
		log.Printf("base dir: %s", dir)
	}
	fileLines, _ := utils.ReadLinesFS(AppFs, outputFilePath)
	results := parser.ParseLstEstimationFile(fileLines)
	if summaryTree {
		jsonRes, _ := json.MarshalIndent(results, "", "\t")
		fmt.Printf("%s\n", jsonRes)
	} else {
		results.Summary()
	}

}
func init() {
	RootCmd.AddCommand(summaryCmd)
	summaryCmd.Flags().BoolVar(&summaryTree, "tree", false, "show a json tree of parsed results from the lst file instead of summary tables")
}
