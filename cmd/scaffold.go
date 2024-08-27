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

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/spf13/afero"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func scaffold(cmd *cobra.Command, args []string) error {
	if debug {
		viper.Debug()
	}

	dir, _ := filepath.Abs(".")

	if viper.GetString("cacheDir") != "" {
		cache := filepath.Clean(filepath.Join(dir, viper.GetString("cacheDir")))
		if preview {
			fmt.Printf("would create cache dir at: %s\n", cache)

			return nil
		}

		AppFs := afero.NewOsFs()

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
		if err = utils.WriteLinesFS(AppFs, []string{
			"*",
			"*/",
			"!.gitignore",
			"",
		}, filepath.Join(cache, ".gitignore")); err != nil {
			return err
		}
	}

	return nil
}

func NewScaffoldCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "scaffold",
		Short: "Scaffold directory structures",
		Long: `This subcommand writes a .gitignore file to the directory specified by
--cacheDir that tells Git to ignore all files in the directory except for the
.gitignore file itself.`,
		RunE: scaffold,
	}

	cmd.Flags().String("cacheDir", "", "create cache directory at path/name")
	errpanic(viper.BindPFlag("cacheDir", cmd.Flags().Lookup("cacheDir")))

	return cmd
}
