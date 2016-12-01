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
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var (
	cleanLvl int
)

// runCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run",
	Short: "run a model",
	Long: `run a model 
    here is more information`,
	Run: func(cmd *cobra.Command, args []string) {
		// TODO: Work your own magic here
		if cleanLvl >= 0 {
			viper.Set("cleanLvl", cleanLvl)
		}
		fmt.Println("run called with clean level of ", viper.GetInt("cleanLvl"))
		fmt.Println("run called with args", strings.Join(args, " "))
		if verbose {
			fmt.Println("called with verbose flag!")
		}
	},
}

func init() {
	RootCmd.AddCommand(runCmd)
	runCmd.Flags().IntVarP(&cleanLvl, "cleanLvl", "c", -1, "clean level used for files")
	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// runCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// runCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")

}
