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
	"io"
	"math/rand"
	"os"
	"path"
	"path/filepath"
	"sync"
	"time"

	"bbi/configlib"

	"github.com/metrumresearchgroup/turnstile"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// VERSION is the current bbi version.
var (
	VERSION string = "develop"
)

var (
	// name of config file.
	// TODO: remove cfgFile if we find no use.
	_/*cfgFile*/ string
	// verbose is whether to give verbose output.
	verbose bool
	debug   bool
	threads int
	// Json indicates whether we should have a JSON tree of output.
	Json               bool
	preview            bool
	executionWaitGroup sync.WaitGroup
)

// RootCmd represents the base command when called without any subcommands.
var RootCmd = &cobra.Command{
	Use:   "bbi",
	Short: "manage and execute models",
	Long:  fmt.Sprintf("bbi CLI version %s", VERSION),
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
	// Set random for application
	rand.Seed(time.Now().UnixNano())

	cobra.OnInitialize(initConfig)

	// Removed "." To avoid IDEs not displaying or typical ignore patterns dropping it.
	viper.SetConfigName("bbi")
	viper.SetConfigType("yaml")

	// Here you will define your flags and configuration settings.
	// Cobra supports Persistent Flags, which, if defined here,
	// will be global for your application.
	RootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "verbose output")
	RootCmd.PersistentFlags().BoolVarP(&debug, "debug", "d", false, "debug mode")
	errpanic(viper.BindPFlag("debug", RootCmd.PersistentFlags().Lookup("debug"))) // Bind Debug to viper
	RootCmd.PersistentFlags().IntVar(&threads, "threads", 4, "number of threads to execute with locally or nodes to execute on in parallel")
	errpanic(viper.BindPFlag("threads", RootCmd.PersistentFlags().Lookup("threads"))) // Update to make sure viper binds to the flag
	RootCmd.PersistentFlags().BoolVar(&Json, "json", false, "json tree of output, if possible")
	errpanic(viper.BindPFlag("json", RootCmd.PersistentFlags().Lookup("json"))) // Bind to viper
	RootCmd.PersistentFlags().BoolVarP(&preview, "preview", "p", false, "preview action, but don't actually run command")
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	if err := configlib.LoadGlobalConfig("bbi"); err != nil {
		log.Fatalf("initializing config %s", err.Error())
	}
}

// TODO: remove flagChanged if we find no use
/* func flagChanged(flags *flag.FlagSet, key string) bool {
	flag := flags.Lookup(key)
	if flag == nil {
		return false
	}

	return flag.Changed
}*/

// Assumes random has been set previously and seeded to avoid reproducible data sets
// Here random is set during root.go setup.
func randomFloat(min int, max int) float64 {
	return float64(float64(min) + rand.Float64()*(float64(max)-float64(min)))
}

func logSetup(config configlib.Config) {
	// Set Logrus level if we're debug
	if config.Debug {
		log.Info("Setting logging to DEBUG")
		log.SetLevel(log.DebugLevel)
	}

	if config.JSON {
		log.Debugf("Setting logrus output formatter to JSON")
		log.SetFormatter(&log.JSONFormatter{})
	}

	if len(config.Logfile) > 0 {
		log.Debugf("A logfile has been specified at %s", config.Logfile)
		logfile := config.Logfile

		// If the path is relative
		if !path.IsAbs(config.Logfile) {
			log.Debugf("The config file specified at %s appears to be relatively referenced", config.Logfile)
			whereami, err := os.Getwd()
			if err != nil {
				log.Fatalf("Unable to get current directory! Details are mysteriously: %s", err)
			}

			log.Debugf("Updating to use log file of %s", filepath.Join(whereami, config.Logfile))
			logfile = filepath.Join(whereami, config.Logfile)
		}

		fs := afero.NewOsFs()
		var outfile afero.File

		if ok, _ := afero.Exists(fs, logfile); ok {
			of, err := fs.OpenFile(logfile, os.O_APPEND|os.O_WRONLY, 0755)

			if err != nil {
				log.Fatalf("Unable to open file at %s. Error is %s", logfile, err)
			}
			outfile = of
		} else {
			// Doesn't exist. Let's create
			of, err := fs.Create(logfile)
			if err != nil {
				log.Fatalf("Error creating new log file located at %s. Details are: %s", logfile, err)
			}
			outfile = of
		}

		tee := io.MultiWriter(outfile, os.Stdout)
		log.SetOutput(tee)
	}
}

// RecordConcurrentError handles the processing of cancellation messages as well placing concurrent errors onto the stack.
func RecordConcurrentError(model string, notes string, err error, channels *turnstile.ChannelMap, cancel chan bool, executor PostWorkExecutor) {
	cancel <- true
	channels.Errors <- newConcurrentError(model, notes, err)

	PostWorkExecution(executor, model, channels, cancel, false, err)
}
