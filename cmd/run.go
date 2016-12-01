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
	flag "github.com/spf13/pflag"
	"github.com/spf13/viper"
)

var (
	cleanLvl     int
	copyLvl      int
	gitignoreLvl int
	git          bool
)

// runCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run",
	Short: "run a (set of) models",
	Long: `run model(s), for example: 
nmu run run001.mod
nmu run run001.mod run002.mod --cleanLvl=1  
 `,
	Run: run,
}

func run(cmd *cobra.Command, args []string) {
	if flagChanged(cmd.Flags(), "cleanLvl") {
		viper.Set("cleanLvl", cleanLvl)
	}
	if flagChanged(cmd.Flags(), "copyLvl") {
		viper.Set("copyLvl", copyLvl)
	}
	if flagChanged(cmd.Flags(), "git") {
		viper.Set("git", git)
	}
	fmt.Println("usegit: ", viper.GetBool("git"))
	fmt.Println("run called with clean level of ", viper.GetInt("cleanLvl"))
	fmt.Println("run called with args", strings.Join(args, " "))
	if verbose {
		fmt.Println("called with verbose flag!")
	}
}
func init() {
	RootCmd.AddCommand(runCmd)
	runCmd.Flags().IntVar(&cleanLvl, "cleanLvl", 0, "clean level used for file output from a given (set of) runs")
	runCmd.Flags().IntVar(&copyLvl, "copyLvl", 0, "copy level used for file output from a given (set of) runs")
	runCmd.Flags().IntVar(&gitignoreLvl, "gitignoreLvl", 0, "gitignore lvl for a given (set of) runs")
	runCmd.Flags().BoolVar(&git, "git", false, "whether git is used")
	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// runCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// runCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")

}

func flagChanged(flags *flag.FlagSet, key string) bool {
	flag := flags.Lookup(key)
	if flag == nil {
		return false
	}
	return flag.Changed
}
