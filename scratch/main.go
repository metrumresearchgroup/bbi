package main

import (
	"fmt"
	"time"

	parser "bbi/parsers/nmparser"
	"bbi/utils"
	"github.com/kr/pretty"
	"github.com/mitchellh/go-homedir"
)

func main() {
	path, err := homedir.Expand("~/Downloads/5039.1/model/etras_popPK/5039/5039.1/5039.1.ext")
	if err != nil {
		panic(err)
	}
	start := time.Now()
	res, err := utils.ReadParamsAndOutputFromExt(path)
	if err != nil {
		panic(err)
	}
	extData, pn := parser.ParseExtData(parser.ParseExtLines(res))
	fmt.Println(time.Since(start))
	pretty.Print(extData[len(extData)-1])
	pretty.Print(pn)

	return
}
