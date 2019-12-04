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
	"bufio"
	"bytes"
	"errors"
	"path"
	"strings"
	"text/template"

	parser "github.com/metrumresearchgroup/babylon/parsers/nmparser"
	"github.com/metrumresearchgroup/babylon/utils"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
)

//scriptTemplate is a go template we'll use for generating the script to do the work.
const scriptTemplate string = `#!/bin/bash

#$ -wd {{ .WorkingDirectory }}

{{ .Command }}
`

var (
	cacheDir     string
	cacheExe     string
	saveExe      string
	cleanLvl     int
	copyLvl      int
	gitignoreLvl int
	git          bool
	//oneEst       bool
	outputDir string
	overwrite bool
)

// RunCmd represents the run command
var nonmemCmd = &cobra.Command{
	Use:   "nonmem",
	Short: "nonmem a (set of) models locally or on the grid",
	Long: `run nonmem model(s), for example: 
bbi nonmem <local|sge> run001.mod
bbi nonmem  --cleanLvl=1 <local|sge> run001.mod run002.mod
bbi nonmem <local|sge> run[001:006].mod // expand to run001.mod run002.mod ... run006.mod local
bbi nonmem <local|sge> .// run all models in directory
 `,
	Run: nonmem,
}

func nonmem(cmd *cobra.Command, args []string) {
	//Just a placeholder
}

// "Copies" a file by reading its content (optionally updating the path)
func copyFileToDestination(l localModel, modifyPath bool) error {

	fs := afero.NewOsFs()

	if exists, _ := afero.DirExists(fs, l.OutputDir); !exists {
		//Create the directory
		fs.MkdirAll(l.OutputDir, 0750)
	}

	//Get the lines of the file
	sourceLines, err := getFileLines(l.ModelPath)

	if err != nil {
		return errors.New("Unable to read the contents of " + l.ModelPath)
	}

	//We'll use stats for setting the mode of the target file to make sure perms are the same
	stats, err := fs.Stat(l.ModelPath)

	if err != nil {
		return err
	}

	//If set to modify, let's look for a $DATA line and replace it
	if modifyPath {
		for k, line := range sourceLines {
			if strings.Contains(line, "$DATA") {
				sourceLines[k] = parser.AddPathLevelToData(line)
			}
		}
	}

	//Write the file contents
	fileContents := strings.Join(sourceLines, "\n")

	afero.WriteFile(fs, path.Join(l.OutputDir, l.ModelName), []byte(fileContents), stats.Mode())

	return nil
}

//Read the contents of a file into a slice of strings
func getFileLines(sourceFile string) ([]string, error) {
	fs := afero.NewOsFs()
	var lines []string

	afero.ReadFile(fs, sourceFile)
	file, err := fs.Open(sourceFile)

	if err != nil {
		return []string{}, err
	}

	defer file.Close()

	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	return lines, nil
}

//processes any template (inlcuding the const one here) to create a byte slice of the entire file
func generateScript(fileTemplate string, l localModel) ([]byte, error) {

	t, err := template.New("file").Parse(fileTemplate)
	buf := new(bytes.Buffer)
	if err != nil {
		return []byte{}, errors.New("There was an error processing the provided script template")
	}

	type content struct {
		WorkingDirectory string
		Command          string
	}

	err = t.Execute(buf, content{
		WorkingDirectory: l.OutputDir,
		Command:          buildNonMemCommandString(l),
	})

	if err != nil {
		return []byte{}, errors.New("An error occured during the execution of the provided script template")
	}

	return buf.Bytes(), nil
}

func buildNonMemCommandString(l localModel) string {

	// TODO: Implement cache
	noBuild := false

	fileName, fileExt := utils.FileAndExt(l.ModelName)
	nmExecutable := l.Settings.NmExecutableOrPath
	cmdArgs := []string{
		strings.Join([]string{fileName, fileExt}, ""),
		strings.Join([]string{fileName, ".lst"}, ""),
	}

	if noBuild {
		cmdArgs = append(cmdArgs, "--nobuild")
	}

	return nmExecutable + " " + strings.Join(cmdArgs, " ")
}
