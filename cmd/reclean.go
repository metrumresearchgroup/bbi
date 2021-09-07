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

	"bbi/runner"

	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// cleanCmd represents the clean command.
var recleanCmd = &cobra.Command{
	Use:   "reclean",
	Short: "clean files in an estimation directory by clean level",
	Long: `
	bbi reclean run001_est_01
 `,
	RunE: reclean,
}

func reclean(cmd *cobra.Command, args []string) error {
	if debug {
		viper.Debug()
	}
	AppFs := afero.NewOsFs()
	err := runner.CleanEstFolder(AppFs, args[0], []string{}, viper.GetInt("clean_lvl"), verbose, debug, preview)
	if err != nil {
		fmt.Printf("err: %v", err)
	}

	return nil
}
func init() {
	nonmemCmd.AddCommand(recleanCmd)

	recleanCmd.Flags().Int("recleanLvl", 0, "clean level to apply")
	errpanic(viper.BindPFlag("recleanLvl", recleanCmd.Flags().Lookup("recleanLvl")))
}
