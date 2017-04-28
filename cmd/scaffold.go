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

	"github.com/dpastoor/babylon/utils"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// scaffoldCmd represents the clean command
var scaffoldCmd = &cobra.Command{
	Use:   "scaffold",
	Short: "scaffold directory structures",
	Long: `
	nmu scaffold --cacheDir=nmcache

	nmu scaffold --cacheDir=../nmcache --preview // show where the cache dir would be created
 `,
	RunE: scaffold,
}

func scaffold(cmd *cobra.Command, args []string) error {
	if debug {
		viper.Debug()
	}

	AppFs := afero.NewOsFs()

	dir, _ := filepath.Abs(".")

	if cacheDir != "" {
		cache := filepath.Clean(filepath.Join(dir, cacheDir))
		if preview {
			fmt.Println(fmt.Sprintf("would create cache dir at: %s", cache))
			return nil
		}

		exists, err := afero.Exists(AppFs, cache)
		if err != nil {
			log.Fatalf("error checking for cache directory: %s", err)
		}
		if exists {
			fmt.Printf("directory already exists at:  %s, nothing to do...\n", cache)
		}

		err = AppFs.MkdirAll(cache, 0766)
		if err != nil {
			log.Fatalf("error creating cache directory: %s", err)
		}
		fmt.Println("adding .gitignore to cache directory...")
		utils.WriteLinesFS(AppFs, []string{
			"*",
			"*/",
			"!.gitignore",
			"",
		},
			filepath.Join(cache, ".gitignore"))
	}

	return nil
}
func init() {
	RootCmd.AddCommand(scaffoldCmd)
	scaffoldCmd.Flags().StringVar(&cacheDir, "cacheDir", "", "create cache directory at path/name")
}
