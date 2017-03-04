package utils

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// ExpandNameSequence takes a full name pattern with a sequence and returns all values
//  run[001:004].mod --> run001.mod run002.mod run003.mod run004.mod
func ExpandNameSequence(fnp string) ([]string, error) {
	var output []string
	r := regexp.MustCompile("(.*)?\\[(.*)\\](.*)?")

	grps := r.FindAllStringSubmatch(fnp, 1)
	if len(grps) > 0 {
		match := grps[0]
		// should be 4 groups
		// 0 is entire expression
		// 1 is anything prior to pattern
		// 2 is pattern
		// 3 is anything after
		// if a group isn't matched, will be empty string
		startName := match[1]
		endName := match[3]
		runSeq, err := ExpandSequence(match[2])
		if err != nil {
			return output, err
		}
		for _, s := range runSeq {
			output = append(output, fmt.Sprintf("%s%s%s", startName, s, endName))
		}
	}
	return output, nil
}

// ExpandSequence expands an integer sequence separated by a colon
// it also understands and will respect padding
// 1:3 --> 1, 2, 3
// 008:011 --> 008, 009, 010, 011
func ExpandSequence(seq string) ([]string, error) {
	var output []string
	splitSeq := strings.Split(seq, ":")
	// get padding
	padding := len(splitSeq[0])
	startNum, err := strconv.Atoi(splitSeq[0])
	if err != nil {
		return []string{}, err
	}
	endNum, err := strconv.Atoi(splitSeq[1])
	if err != nil {
		return []string{}, err
	}
	if startNum < endNum {
		for i := startNum; i <= endNum; i++ {
			output = append(output, PadNum(i, padding))
		}
	} else {
		for i := startNum; i >= endNum; i-- {
			output = append(output, PadNum(i, padding))
		}
	}
	return output, nil
}
