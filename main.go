package main

import (
	"fmt"
	"regexp"
	"strings"
)

func main() {
	testStrings := []string{
		"hello here is a FILE",
		"$TABLE CL V FILE=patab001",
		"$TABLE O O2 FILE = sdtab001",
		"$TABLE O O2 FILE = edtab001.ext",
		"$TABLE O O2 FILE = mdtab001.ext MORE",
	}
	// pattern FILE, maybe whitespace, =, maybe whitespace, word (non-whitespace)
	r, _ := regexp.Compile("FILE\\s?=\\s?(\\S+)")
	for _, line := range testStrings {
		fmt.Println(strings.Contains(line, "FILE="))
		match := r.FindStringSubmatch(line)
		if len(match) > 0 {
			fmt.Println(match[1])
		}
	}
}
