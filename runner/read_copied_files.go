package runner

import (
	"encoding/json"
	"fmt"

	"github.com/spf13/afero"
)

// ReadCopiedFiles reads the _copied.json file for given run
// eg ReadCopiedFiles(fs, "run001") --> looks to unmarshal run001_copied.json.
func ReadCopiedFiles(fs afero.Fs, run string) ([]TargetedFile, error) {
	bytes, err := afero.ReadFile(fs, fmt.Sprintf("%s_copied.json", run))
	if err != nil {
		return nil, err
	}
	var copiedFiles []TargetedFile
	err = json.Unmarshal(bytes, &copiedFiles)
	if err != nil {
		return nil, err
	}

	return copiedFiles, nil
}

// GetCopiedFilenames is a wrapper around ReadCopiedFiles
// and returns the filenames for all copied files for a given run.
func GetCopiedFilenames(fs afero.Fs, run string) ([]string, error) {
	var fileStrings []string
	files, err := ReadCopiedFiles(fs, run)
	if err != nil {
		return nil, err
	}
	for _, file := range files {
		fileStrings = append(fileStrings, file.File)
	}

	return fileStrings, nil
}
