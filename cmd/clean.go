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
	"strings"

	"bbi/runner"
	"bbi/utils"
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

// cleanCmd represents the clean command
var cleanCmd = &cobra.Command{
	Use:   "clean",
	Short: "clean files and folders",
	Long: `
glob examples:
bbi clean *.mod // anything with extension .mod
bbi clean *.mod --noFolders // anything with extension .mod
bbi clean run* // anything starting with run
regular expression examples:

bbi clean ^run --regex // anything beginning with the letters run
bbi clean ^run -v --regex // print out files and folders that will be deleted 
bbi clean ^run --filesOnly --regex // only remove matching files 
bbi clean _est_ --dirsOnly --regex // only remove matching folders  
bbi clean _est_ --dirsOnly --preview --regex // show what output would be if clean occured but don't actually clean 
bbi clean "run009.[^mod]" --regex // all matching run009.<ext> but not .mod files
bbi clean "run009.(mod|lst)$" --regex // match run009.lst and run009.mod

can also clean via the opposite of a match with inverse

bbi clean ".modt{0,1}$" --filesOnly --inverse --regex // clean all files not matching .mod or .modt

clean copied files via

bbi clean --copiedRuns="run001"
bbi clean --copiedRuns="run[001:010]"

can be a comma separated list as well

bbi clean --copiedRuns="run[001:010],run100"
 `,
	RunE: clean,
}

func clean(cmd *cobra.Command, args []string) error {
	if debug {
		viper.Debug()
	}

	AppFs := afero.NewOsFs()

	dir, _ := filepath.Abs(".")
	dirInfo, err := afero.ReadDir(AppFs, dir)
	if err != nil {
		return fmt.Errorf("error finding dir (%s)", err)
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

	//TODO: Have this operate now based on model name?
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
			AppFs.RemoveAll(filepath.Join(dir, f))
		}
	}
	if !dirsOnly {
		for _, f := range matchedFiles {
			AppFs.Remove(filepath.Join(dir, f))
		}
	}

	return nil
}

func getMatches(s []string, expr string, regex bool) ([]string, error) {
	var matches []string
	var err error
	if regex {
		if !inverse {
			matches, err = utils.ListMatchesByRegex(s, expr)
		} else {
			matches, err = utils.ListNonMatchesByRegex(s, expr)
		}
		if err != nil {
			return nil, fmt.Errorf("error with regex (%s), err: (%s)", expr, err)
		}
	} else {
		if !inverse {
			matches, err = utils.ListMatchesByGlob(s, expr)
		} else {
			matches, err = utils.ListNonMatchesByGlob(s, expr)
		}
		if err != nil {
			return nil, fmt.Errorf("error with glob (%s), err: (%s)", expr, err)
		}
	}

	return matches, nil
}

func init() {
	nonmemCmd.AddCommand(cleanCmd)
	cleanCmd.Flags().BoolVar(&dirsOnly, "dirsOnly", false, "only match and clean directories")
	cleanCmd.Flags().BoolVar(&filesOnly, "filesOnly", false, "only match and clean files")
	cleanCmd.Flags().BoolVar(&inverse, "inverse", false, "inverse selection from the given regex match criteria")
	cleanCmd.Flags().BoolVar(&regex, "regex", false, "use regular expression to match instead of glob")
	cleanCmd.Flags().StringVar(&copiedRuns, "copiedRuns", "", "run names")
}
