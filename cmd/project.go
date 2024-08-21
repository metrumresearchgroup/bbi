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
	"errors"
	"os"
	"path/filepath"

	"github.com/olekukonko/tablewriter"

	"strings"

	parser "github.com/metrumresearchgroup/bbi/parsers/nmparser"

	"github.com/metrumresearchgroup/bbi/utils"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func probs(_ *cobra.Command, args []string) error {
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
		return errors.New("project only supports specifying one directory")
	}

	dir, _ := filepath.Abs(dirPath)

	modelFiles, err := utils.ListModels(dir, ".mod", AppFs)
	if err != nil {
		return err
	}

	filesAbs := make([]string, len(modelFiles))
	for i := range modelFiles {
		filesAbs[i] = filepath.Join(dirPath, modelFiles[i])
	}

	modelSummaries := modSummaries(AppFs, filesAbs)

	if Json {
		err = utils.PrintJSON(modelSummaries)
		if err != nil {
			log.Fatal(err)
		}
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
func NewProbsCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "probs [flags] [<directory>]",
		Short: "Summarize model definitions in a directory",
		Long: `This subcommand extracts information from the *.mod files in the
current directory or the specified directory. By default, it displays a
table with the $PROBLEM text for each model file. If --json is passed, it
prints JSON output with more details about each model (e.g., which
estimation methods are present).

Note: Only model files with a *.mod extension are currently supported.`,
		Example: `# Output a table summarizing the $PROBLEM text for the *.mod files
# in the current directory
bbi nonmem probs
# Instead of the table, show JSON output with more details
bbi nonmem probs --json`,
		RunE: probs,
	}
}

type runSummary struct {
	RunName string
	Ok      bool
	Summary parser.ModelInfo
}

func modSummaries(AppFs afero.Fs, files []string) []runSummary {
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

// probSummary prints the problem statements from each model.
func probSummary(mp []modelSummary) {
	table := tablewriter.NewWriter(os.Stdout)
	table.SetAlignment(tablewriter.ALIGN_LEFT)
	table.SetColWidth(100)
	table.SetHeader([]string{"Run", "Prob"})

	for _, m := range mp {
		table.Append([]string{m.ModelName, strings.TrimPrefix(m.Prob, "$PROB")})
	}
	table.Render()
}
