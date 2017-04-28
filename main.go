package main

import (
	"fmt"

	"github.com/dpastoor/babylon/utils"
)

func main() {
	testStrings := []string{
		"run[001:003].mod",
		"[0:2]hello",
		"tab[0:2]",
		"run[01:01].mod",
		"run[005:002].mod",
		"run000.mod",
	}
	for _, t := range testStrings {
		fmt.Println("------------")
		fmt.Println("string: ", t)
		expandedNames, err := utils.ExpandNameSequence(t)
		fmt.Println("err:", err)
		fmt.Println(expandedNames)
		fmt.Println("------------")
	}
}
