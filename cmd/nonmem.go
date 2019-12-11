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
	"fmt"
	"io/ioutil"
	"log"
	"path"
	"strings"
	"text/template"

	parser "github.com/metrumresearchgroup/babylon/parsers/nmparser"
	"github.com/metrumresearchgroup/babylon/runner"
	"github.com/spf13/afero"
	"github.com/spf13/cobra"
)

//scriptTemplate is a go template we'll use for generating the script to do the work.
const scriptTemplate string = `#!/bin/bash

#$ -wd {{ .WorkingDirectory }}

{{ .Command }}
`

//NonMemModel is the definition of a model for NonMem including its target directories and settings required for execution
type NonMemModel struct {
	//Model is the name of the model on which we will action: acop.mod
	Model string `json:"model_name"`
	//Path is the Fully Qualified Path to the original model
	Path string `json:"model_path"`
	//FileName is the Filename component (sans extension)
	FileName string `json:"model_filename"`
	//Extension is the extension of the file
	Extension string `json:"model_extension"`
	//OriginalPath is the path at which the original model was located: /Users/Documents/acop/
	OriginalPath string `json:"original_path"`
	//OutputDir is the directory into which the copied models and work will be located
	OutputDir string `json:"output_dir"`
	//Settings are basically the cobra definitions / requirements for the iteration
	Settings runner.RunSettings `json:"settings"`
	//Whether or not the model had an error on generation or execution
	Error error `json:"error"`
}

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
	println(runLongDescription)
}

func init() {
	RootCmd.AddCommand(nonmemCmd)
}

// "Copies" a file by reading its content (optionally updating the path)
func copyFileToDestination(l NonMemModel, modifyPath bool) error {

	fs := afero.NewOsFs()

	if exists, _ := afero.DirExists(fs, l.OutputDir); !exists {
		//Create the directory
		fs.MkdirAll(l.OutputDir, 0750)
	}

	//Get the lines of the file
	sourceLines, err := getFileLines(l.Path)

	if err != nil {
		return errors.New("Unable to read the contents of " + l.Path)
	}

	//We'll use stats for setting the mode of the target file to make sure perms are the same
	stats, err := fs.Stat(l.Path)

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

	afero.WriteFile(fs, path.Join(l.OutputDir, l.Model), []byte(fileContents), stats.Mode())

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
func generateScript(fileTemplate string, l NonMemModel) ([]byte, error) {

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

func buildNonMemCommandString(l NonMemModel) string {

	// TODO: Implement cache
	noBuild := false
	nmExecutable := l.Settings.NmExecutableOrPath
	cmdArgs := []string{
		l.OutputDir + "/" + l.Model,
		"",
		l.OutputDir + "/" + l.FileName + ".lst",
		"",
	}

	if noBuild {
		cmdArgs = append(cmdArgs, "--nobuild")
	}

	return nmExecutable + " " + strings.Join(cmdArgs, " ")
}

//modelName is the full file + ext representation of the model (ie acop.mod)
//directory is the directory in which we will be performing cleanup
//exceptions is a variadic input for allowing exceptions / overrides.
//We're taking a local model because grid engine execution doesn't wait for completion. Nothing to do :)
func filesToCleanup(model NonMemModel, exceptions ...string) runner.FileCleanInstruction {
	fci := runner.FileCleanInstruction{
		Location: model.OutputDir,
	}

	files := getActionableFileList(model.FileName, model.Settings.CleanLvl, model.OutputDir)

	for _, v := range files {
		if !isFilenameInExceptions(exceptions, v) {
			//Let's add it to the cleanup list if it's not in the exclusions
			fci.FilesToRemove = append(fci.FilesToRemove, newTargetFile(v, model.Settings.CleanLvl))
		}
	}

	return fci
}

func isFilenameInExceptions(haystack []string, needle string) bool {

	for _, v := range haystack {
		if v == needle {
			return true
		}
	}

	//No matches
	return false
}

func newTargetFile(filename string, level int) runner.TargetedFile {
	return runner.TargetedFile{
		File:  filename,
		Level: level,
	}
}

func filesToCopy(model NonMemModel, mandatoryFiles ...string) runner.FileCopyInstruction {
	fci := runner.FileCopyInstruction{
		CopyTo:   model.OriginalPath,
		CopyFrom: model.OutputDir,
	}

	//Process mandatory files first
	for _, v := range mandatoryFiles {
		//Crank it up to 11
		fci.FilesToCopy = append(fci.FilesToCopy, newTargetFile(v, 11))
	}

	//Create Target File Entries
	for _, v := range getActionableFileList(model.FileName, model.Settings.CopyLvl, model.OutputDir) {
		fci.FilesToCopy = append(fci.FilesToCopy, newTargetFile(v, model.Settings.CopyLvl))
	}

	return fci
}

func getActionableFileList(filename string, level int, filepath string) []string {
	var output []string
	files := make(map[int][]string)

	//These files are files that may be desired above the normal.
	files[2] = []string{
		"background.set",
		"compile.lnk",
		"FCON",
		"FDATA",
		"FMSG",
		"FREPORT",
		"FSIZES",
		"FSTREAM",
		"FSUBS",
		"FSUBS.0",
		"FSUBS.o",
		"FSUBS_MU.F90",
		"FSUBS.f90",
		"fsubs.f90",
		"FSUBS2",
		"gfortran.txt",
		"GFCOMPILE.BAT",
		"INTER",
		"licfile.set",
		"linkc.lnk",
		"LINK.LNK",
		"LINKC.LNK",
		"locfile.set",
		"maxlim.set",
		"newline",
		"nmexec.set",
		"nmpathlist.txt",
		"nmprd4p.mod",
		"nobuild.set",
		"parafile.set",
		"parafprint.set",
		"prcompile.set",
		"prdefault.set",
		"prsame.set",
		"PRSIZES.f90",
		"rundir.set",
		"runpdir.set",
		"simparon.set",
		"temp_dir",
		"tprdefault.set",
		"trskip.set",
		"worker.set",
		"xmloff.set",
		"fort.2001",
		"fort.2002",
		"flushtime.set",
	}

	//Get the files from the model
	//Look specifically for .mod file
	fileContents, err := ioutil.ReadFile(path.Join(filepath, filename+".mod"))
	fileLines := strings.Split(string(fileContents), "\n")

	if err != nil {
		//Let the user know this is basically a no-op
		log.Printf("We could not locate or read the mod file (%s) indicated to locate output files. As such none will be included in copy / delete operations", filename+".mod")
	}

	//Add defined output files to the list at level 1
	files[1] = append(files[1], parser.FindOutputFiles(fileLines)...)

	//Level two will parse the mod file and return those outputs as Level 1

	for i := 0; i <= level; i++ {
		if val, ok := files[i]; ok {
			output = append(output, val...)
		}
	}

	//Extensions now
	output = append(output, extrapolateFilesFromExtensions(filename, level, filepath)...)

	return output
}

// Extrapolate extensions into string representations of filenames.
func extrapolateFilesFromExtensions(filename string, level int, filepath string) []string {
	var output []string
	extensions := make(map[int][]string)

	// parser now needs all these files + other tooling uses xml files
	extensions[1] = []string{
		".xml",
		".grd",
		".shk",
		".cor",
		".cov",
		".ext",
		".lst",
	}

	extensions[2] = []string{
		".clt",
		".coi",
		".clt",
		".coi",
		".cpu",
		".shm",
		".phi",
	}

	extensions[3] = []string{
		"",
		"_ETAS",
		"_RMAT",
		"_SMAT",
		".msf",
		"_ETAS.msf",
		"_RMAT.msf",
		"_SMAT.msf",
	}

	//Loop, extrapolate and append to the output slice
	for i := 0; i <= level; i++ {
		if val, ok := extensions[i]; ok {
			for _, ext := range val {
				//For each filename, let's trim and compose the contents
				output = append(output, strings.TrimSpace(fmt.Sprintf("%s%s", filename, ext)))
			}
		}
	}

	return output
}

func newPostWorkInstruction(model NonMemModel, cleanupExclusions []string, mandatoryCopyFiles []string) runner.PostWorkInstructions {
	pwi := runner.PostWorkInstructions{}

	pwi.FilesToCopy = filesToCopy(model, mandatoryCopyFiles...)
	pwi.FilesToClean = filesToCleanup(model, cleanupExclusions...)

	return pwi
}
