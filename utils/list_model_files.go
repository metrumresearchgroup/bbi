package utils

import (
	"strings"

	"fmt"

	"github.com/spf13/afero"
)

// ListFilesByExt returns an array of model files given an array of files of any type
func ListFilesByExt(files []string, modExt string) []string {
	var matchFiles []string
	for _, file := range files {
		if strings.HasSuffix(file, modExt) {
			matchFiles = append(matchFiles, file)
		}
	}
	return matchFiles
}

// ListModels returns all model files for a given modExt in a provided dir
func ListModels(dirName string, modExt string, fs afero.Fs) ([]string, error) {
	dirInfo, err := afero.ReadDir(fs, dirName)
	if err != nil {
		return []string{}, fmt.Errorf("error finding model dir (%s)", err)
	}
	filesInDir := ListFiles(dirInfo)
	modelFiles := ListFilesByExt(filesInDir, modExt)
	return modelFiles, nil
}
