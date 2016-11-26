package main

import (
	"fmt"

	"github.com/dpastoor/nonmemutils/utils"
	"github.com/spf13/afero"
)

func main() {
	AppFs := afero.NewOsFs()
	data, _ := utils.ReadLinesFS(AppFs, "parser/fixtures/lstfiles/simple-onecmpt-ex1.lst")
	for i, line := range data {
		fmt.Printf("%v:, %s\n", i, line)
	}
}
