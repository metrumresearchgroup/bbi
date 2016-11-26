package main

import (
	"encoding/json"
	"fmt"

	"github.com/dpastoor/nonmemutils/parser"
	"github.com/dpastoor/nonmemutils/utils"
)

func main() {
	data, _ := utils.ReadLines("parser/fixtures/lstfiles/simple-onecmpt-ex1.lst")
	results := parser.ParseLstEstimationFile(data)
	bs, _ := json.MarshalIndent(results, "", "\t")
	fmt.Println(string(bs))
	results.Summary()
}
