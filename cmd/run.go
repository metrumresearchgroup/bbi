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
	"log"
	"time"

	"sync"

	"github.com/dpastoor/nonmemutils/runner"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var (
	cacheDir     string
	cacheExe     string
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
	RunE: run,
}

func run(cmd *cobra.Command, args []string) error {
	if flagChanged(cmd.Flags(), "cacheDir") {
		viper.Set("cacheDir", cacheDir)
	}
	if flagChanged(cmd.Flags(), "cacheExe") {
		fmt.Println("setting new cache exe of: ", cacheExe)
		viper.Set("cacheExe", cacheExe)
	}
	if flagChanged(cmd.Flags(), "cleanLvl") {
		viper.Set("cleanLvl", cleanLvl)
	}
	if flagChanged(cmd.Flags(), "copyLvl") {
		viper.Set("copyLvl", copyLvl)
	}
	if flagChanged(cmd.Flags(), "git") {
		viper.Set("git", git)
	}
	if flagChanged(cmd.Flags(), "threads") {
		viper.Set("threads", threads)
	}
	if debug {
		viper.Debug()
	}

	AppFs := afero.NewOsFs()
	var wg sync.WaitGroup
	queue := make(chan struct{}, viper.GetInt("threads"))
	start := time.Now()
	if verbose {
		log.Printf("setting up a work queue with %v workers", viper.GetInt("threads"))
	}
	defer close(queue)
	wg.Add(len(args))
	for _, arg := range args {
		log.Printf("starting goroutine for run %s \n", arg)
		go runModel(AppFs, arg, queue, &wg, verbose, debug)
	}

	wg.Wait()
	elapsed := time.Since(start)
	log.Printf("running on %v threads took %s", viper.GetInt("threads"), elapsed)
	return nil
}

func runModel(
	fs afero.Fs,
	filePath string,
	queue chan struct{},
	wg *sync.WaitGroup,
	verbose bool,
	debug bool,
) {
	defer wg.Done()
	queue <- struct{}{}
	if verbose {
		log.Printf("run %s running on worker!", filePath)
	}
	if debug {
		viper.Debug()
	}
	runner.EstimateModel(
		fs,
		filePath,
		verbose,
		debug,
		viper.GetString("cacheDir"),
		viper.GetString("cacheExe"),
	)
	if verbose {
		log.Printf("completed run %s releasing worker back to queue \n", filePath)
	}
	<-queue
}

func init() {
	RootCmd.AddCommand(runCmd)
	runCmd.Flags().StringVar(&cacheDir, "cacheDir", "", "directory path for cache of nonmem executables for NM7.4+")
	runCmd.Flags().StringVar(&cacheExe, "cacheExe", "", "name of executable stored in cache")
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
