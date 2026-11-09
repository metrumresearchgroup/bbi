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
	"io/fs"
	"os"
	"path/filepath"

	"github.com/metrumresearchgroup/bbi/configlib"
	"github.com/metrumresearchgroup/bbi/internal/fmatch"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func reclean(cmd *cobra.Command, args []string) error {
	if debug {
		viper.Debug()
	}
	if len(args) != 1 {
		return fmt.Errorf("must specify one positional argument, a directory")
	}

	outdir, err := filepath.Abs(args[0])
	if err != nil {
		return err
	}
	fi, err := os.Stat(outdir)
	if err != nil {
		return err
	}
	if !fi.IsDir() {
		return fmt.Errorf("not a directory: %s", args[0])
	}

	cfgfile := filepath.Join(outdir, "bbi.yaml")
	config, err := configlib.ReadSpecifiedFileIntoConfigStruct(cfgfile)
	if err != nil {
		return err
	}

	level := config.CleanLvl
	if cmd.Flags().Changed("lvl") {
		level, err = cmd.Flags().GetInt("lvl")
		if err != nil {
			return fmt.Errorf("bug: %w", err)
		}
	}

	logSetup(config)
	log.Infof("re-cleaning at level %d: %s", level, outdir)

	var fn fmatch.Func
	if preview {
		fn = recleanPreview
	} else {
		fn = func(path string, d fs.DirEntry, level int) error {
			return recleanRemove(path, d, level, verbose)
		}
	}

	cleanMatcher := fmatch.NonmemCleanMatcher(config)

	return cleanMatcher.Do(outdir, level, fn)
}

func recleanPreview(path string, d fs.DirEntry, _ int) error {
	fi, err := d.Info()
	if err != nil {
		return err
	}

	kind := "file"
	if fi.IsDir() {
		kind = "directory"
	}

	fmt.Printf("would delete %s %s\n", kind, path)

	return nil
}

func recleanRemove(path string, _ fs.DirEntry, _ int, verbose bool) error {
	err := os.RemoveAll(path)
	if err != nil {
		return err
	}

	if verbose {
		fmt.Printf("deleted %s\n", path)
	}

	return nil
}

func NewRecleanCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "reclean [flags] <run dir>",
		Short: "Clean files in run directory by specified level",
		Example: `  # Reclean the run directory for 001, deleting what would have been
  # cleaned if the model had been executed with a clean level of 1.
  bbi nonmem run local --clean_lvl=0 001.ctl
  bbi nonmem reclean --lvl=1 001

  # When a 'bbi nonmem run' call fails (e.g., due to a non-zero exit
  # of the underlying nmfe call), the cleanup logic is not executed.
  # You can follow up with a call to 'bbi nonmem reclean' to trigger
  # the cleanup logic, using the clean level that was configured for
  # that run.
  bbi nonmem run local 001.ctl # nmfe errors
  bbi nonmem reclean 001`,
		RunE:         reclean,
		SilenceUsage: true,
	}

	cmd.Flags().Int("lvl", 1, "level at which to clean the run directory")

	return cmd
}
