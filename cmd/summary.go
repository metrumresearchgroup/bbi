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

	"github.com/dpastoor/nonmemutils/parser"
	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

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
	jsonRes, _ := json.MarshalIndent(results, "", "\t")
	fmt.Printf("%s", jsonRes)
	results.Summary()

}
func init() {
	RootCmd.AddCommand(summaryCmd)
	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// runCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// runCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")

}
