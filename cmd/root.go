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
	"os"

	"github.com/metrumresearchgroup/babylon/configlib"
	"github.com/spf13/cobra"
	flag "github.com/spf13/pflag"
)

// VERSION is the current bbi version
var VERSION string = "2.0.0-alpha.3"

var (
	// name of config file
	cfgFile string
	// verbose is whether to give verbose output
	verbose bool
	debug   bool
	threads int
	Json    bool
	preview bool
	noExt   bool
	noGrd   bool
	noCov   bool
	noCor   bool
	noShk   bool
)

// RootCmd represents the base command when called without any subcommands
var RootCmd = &cobra.Command{
	Use:   "bbi",
	Short: "manage and execute models",
	Long:  fmt.Sprintf("babylon cli version %s", VERSION),
}

// Execute adds all child commands to the root command sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute(build string) {
	if build != "" {
		VERSION = fmt.Sprintf("%s-%s", VERSION, build)
	}
	RootCmd.Long = fmt.Sprintf("bbi cli version %s", VERSION)
	if err := RootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(-1)
	}
}

func init() {
	cobra.OnInitialize(initConfig)

	// Here you will define your flags and configuration settings.
	// Cobra supports Persistent Flags, which, if defined here,
	// will be global for your application.

	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/babylonconfig.toml)")
	RootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "verbose output")
	RootCmd.PersistentFlags().BoolVarP(&debug, "debug", "d", false, "debug mode")
	RootCmd.PersistentFlags().IntVar(&threads, "threads", 0, "number of threads to execute with")
	RootCmd.PersistentFlags().BoolVar(&Json, "json", false, "json tree of output, if possible")
	RootCmd.PersistentFlags().BoolVarP(&preview, "preview", "p", false, "preview action, but don't actually run command")
	RootCmd.PersistentFlags().BoolVarP(&noExt, "no-ext-file", "", false, "do not use ext file")
	RootCmd.PersistentFlags().BoolVarP(&noGrd, "no-grd-file", "", false, "do not use grd file")
	RootCmd.PersistentFlags().BoolVarP(&noCov, "no-cov-file", "", false, "do not use cov file")
	RootCmd.PersistentFlags().BoolVarP(&noCor, "no-cor-file", "", false, "do not use cor file")
	RootCmd.PersistentFlags().BoolVarP(&noShk, "no-shk-file", "", false, "do not use shk file")

	// Cobra also supports local flags, which will only run
	// when this action is called directly.
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	// if cfgFile != "" { // enable ability to specify config file via flag
	// 	viper.SetConfigFile(cfgFile)
	// }
	// TODO: set config a little more flexibly
	err := configlib.LoadGlobalConfig("babylonconfig")
	if err != nil {
		fmt.Println(fmt.Errorf("err initializing config %s", err))
	}
}

func flagChanged(flags *flag.FlagSet, key string) bool {
	flag := flags.Lookup(key)
	if flag == nil {
		return false
	}
	return flag.Changed
}
