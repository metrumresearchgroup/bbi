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
	"path/filepath"
	"regexp"
	"strings"

	"github.com/gobwas/glob"

	"github.com/metrumresearchgroup/bbi/runner"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var (
	filesOnly  bool
	dirsOnly   bool
	inverse    bool
	copiedRuns string
	regex      bool
)

func clean(cmd *cobra.Command, args []string) error {
	if debug {
		viper.Debug()
	}

	AppFs := afero.NewOsFs()

	dir, _ := filepath.Abs(".")
	dirInfo, err := afero.ReadDir(AppFs, dir)
	if err != nil {
		return fmt.Errorf("finding dir: %w", err)
	}
	files := utils.ListFiles(dirInfo)
	folders := utils.ListDirNames(dirInfo)
	matchedFiles := []string{}
	matchedFolders := []string{}
	for _, expr := range args {
		if !filesOnly {
			matches, err := getMatches(folders, expr, regex)
			if err != nil {
				return err
			}
			matchedFolders = append(matchedFolders, matches...)
		}
		if !dirsOnly {
			matches, err := getMatches(files, expr, regex)
			if err != nil {
				return err
			}
			matchedFiles = append(matchedFiles, matches...)
		}
	}

	// TODO: Have this operate now based on model name?
	if copiedRuns != "" {
		copies := strings.Split(copiedRuns, ",")
		for _, arg := range copies {
			pat, err := utils.ExpandNameSequence(arg)
			if err != nil {
				log.Printf("err expanding name: %v", err)
				// don't try to run this model
				continue
			}
			if verbose || debug {
				log.Printf("expanded models: %s \n", pat)
			}
			for _, p := range pat {
				cleanedFiles, err := runner.GetCopiedFilenames(AppFs, p)
				if err != nil {
					log.Printf("err getting copied filenames: %v", err)
					// don't try to run this model
					continue
				}
				matchedFiles = append(matchedFiles, cleanedFiles...)
			}
		}
	}
	if verbose {
		fmt.Println("cleaning files: ", matchedFiles)
		fmt.Println("cleaning folders: ", matchedFolders)
	}
	if preview {
		fmt.Println("would clean files: ", matchedFiles)
		fmt.Println("would clean folders: ", matchedFolders)

		return nil
	}

	if !filesOnly {
		for _, f := range matchedFolders {
			if !filepath.IsAbs(f) {
				f = filepath.Join(dir, f)
			}
			// explicitly ignoring errors here, as this is not critical
			// and it's more important to try than to succeed
			err = AppFs.RemoveAll(f)
			if err != nil {
				log.Printf("failed to remove directory %s: %v", f, err)
			}
		}
	}
	if !dirsOnly {
		for _, f := range matchedFiles {
			if !filepath.IsAbs(f) {
				f = filepath.Join(dir, f)
			}
			// explicitly ignoring errors here, as this is not critical
			// and it's more important to try than to succeed
			err = AppFs.Remove(f)
			if err != nil {
				log.Printf("failed to remove file %s: %v", f, err)
			}
		}
	}

	return nil
}

func getMatches(s []string, expr string, regex bool) ([]string, error) {
	if regex {
		rx, err := regexp.Compile(expr)
		if err != nil {
			return nil, err
		}
		if inverse {
			return utils.ListNonMatchesByRegex(s, rx), nil
		} else {
			return utils.ListMatchesByRegex(s, rx), nil
		}
	} else {
		gb, err := glob.Compile(expr)
		if err != nil {
			return nil, err
		}
		if inverse {
			return utils.ListNonMatchesByGlob(s, gb), nil
		} else {
			return utils.ListMatchesByGlob(s, gb), nil
		}
	}
}

func NewCleanCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "clean [flags] <pattern> [<pattern>...]",
		Short: "Clean files and folders",
		Long: `Clean the files and directories that match the specified patterns.
Whether the pattern is interpreted as a glob or regex is controlled by the
--regex flag.

If files were copied to the parent directory automatically after model
execution (via the copy_lvl configuration), the original files in the run
directory can be cleaned up by selecting the run with the --copiedRuns
option.`,
		Example: `  # Remove items in the current directory that end with ".mod"
  bbi nonmem clean *.mod
  # The same as above but ensure only files are removed
  bbi nonmem clean --filesOnly *.mod

  # Remove files in the current directory that start with "run" and end with
  # ".mod" or ".lst"
  bbi nonmem clean --filesOnly --regex "run.*\.(mod|lst)$"
  # Report what the above would remove but don't actually do it
  bbi nonmem clean --preview --filesOnly --regex "run.*\.(mod|lst)$"

  # Remove copied files (recorded in '{run}_copied.json' by 'bbi run') for
  # run001, run002, run003, and run100
  bbi nonmem clean --copiedRuns='run[001:003],run100'`,
		RunE: clean,
	}

	cmd.Flags().BoolVar(&dirsOnly, "dirsOnly", false, "only match and clean directories")
	cmd.Flags().BoolVar(&filesOnly, "filesOnly", false, "only match and clean files")
	cmd.Flags().BoolVar(&inverse, "inverse", false, "inverse selection from the given regex match criteria")
	cmd.Flags().BoolVar(&regex, "regex", false, "use regular expression to match instead of glob")
	cmd.Flags().StringVar(&copiedRuns, "copiedRuns", "", "run names")

	return cmd
}
