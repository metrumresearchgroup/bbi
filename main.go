package main

import (
	"fmt"

	"github.com/dpastoor/nonmemutils/utils"
)

func main() {
	testStrings := []string{
		"run001.mod",
		"run001.lst",
		"run002.mod",
		"run002.lst",
	}

	matches, err := utils.ListMatchesByRegex(testStrings, "run001\\.mod")
	fmt.Println(matches)
	fmt.Println(err)
}
