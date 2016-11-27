package main

import (
	"fmt"
	"path/filepath"

	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
)

func main() {
	AppFs := afero.NewOsFs()
	filePath := "parser/fixtures/lstfiles/simple-onecmpt-ex1.lst"
	fileName, _ := utils.FileAndExt(filePath)
	dir := filepath.Dir(filePath)
	upOneLevel := filepath.Join(dir, "..")
	dirInfo, _ := afero.ReadDir(AppFs, upOneLevel)
	for i, line := range dirInfo {
		if line.IsDir() {
			fmt.Printf("%v: %s\n", i, line.Name())
		}
	}
}
