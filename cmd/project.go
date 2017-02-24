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
	"errors"
	"fmt"
	"path/filepath"

	"os"

	"github.com/apcera/termtables"
	"github.com/dpastoor/nonmemutils/parser"
	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// probsCmd represents the command to get information about a given modeling project
var probsCmd = &cobra.Command{
	Use:   "probs",
	Short: "summarize information about project",
	Long: `get information about models in the project: 
nmu project
 `,
	RunE: probs,
}

func probs(cmd *cobra.Command, args []string) error {
	if debug {
		viper.Debug()
	}

	AppFs := afero.NewOsFs()
	var dirPath string
	switch len(args) {
	case 0:
		dirPath, _ = os.Getwd()
	case 1:
		dirPath = args[0]
	default:
		fmt.Println("currently only supports scanning one directory")
		return errors.New("project only supports specifying one directory")
	}

	dir, _ := filepath.Abs(dirPath)

	modelFiles, err := utils.ListModels(dir, ".mod", AppFs)
	if err != nil {
		return err
	}
	modelSummaries := modSummaries(AppFs, modelFiles, dir)
	if tree {
		jsonRes, _ := json.MarshalIndent(modelSummaries, "", "\t")
		fmt.Printf("%s\n", jsonRes)
	} else {
		var probSummaries []modelSummary
		for _, ms := range modelSummaries {
			if ms.Ok {
				probSummaries = append(probSummaries, modelSummary{
					ModelName: ms.RunName,
					Prob:      ms.Summary.Prob,
				})
			}
		}
		probSummary(probSummaries)
	}
	return nil
}
func init() {
	RootCmd.AddCommand(probsCmd)
}

type runSummary struct {
	RunName string
	Ok      bool
	Summary parser.ModelInfo
}

func modSummaries(AppFs afero.Fs, files []string, dir string) []runSummary {
	var summaries []runSummary
	for _, file := range files {
		var rs runSummary
		rs.RunName, _ = utils.FileAndExt(file)
		fileLines, _ := utils.ReadLinesFS(AppFs, file)
		modelSummary, err := parser.ParseModInfo(fileLines)
		if err != nil {
			rs.Ok = false
		} else {
			rs.Ok = true
			rs.Summary = modelSummary
		}
		summaries = append(summaries, rs)
	}
	return summaries
}

type modelSummary struct {
	ModelName string
	Prob      string
}

// probSummary prints the problem statements from each model
func probSummary(mp []modelSummary) {
	termtables.DefaultStyle = &termtables.TableStyle{
		SkipBorder: false,
		BorderX:    "-", BorderY: "|", BorderI: "+",
		PaddingLeft: 3, PaddingRight: 3,
		Width:     100,
		Alignment: termtables.AlignRight,
	}
	probTable := termtables.CreateTable()
	probTable.AddHeaders("Run", "Prob")
	for _, m := range mp {
		probTable.AddRow(m.ModelName, m.Prob)
	}
	probTable.SetAlign(termtables.AlignLeft, 1)

	fmt.Println(probTable.Render())
}
