package utils

import (
	"strconv"
	"strings"
)

// LeftPad pads a given string a number of times
func LeftPad(s string, padStr string, pLen int) string {
	return strings.Repeat(padStr, pLen) + s
}

// PadNum is a 'specialized' version of LeftPad to pad run numbers
func PadNum(r int, padLength int) string {
	runString := strconv.Itoa(r)
	numToPad := padLength - len(runString)
	if numToPad <= 0 {
		return runString
	}

	return LeftPad(runString, "0", numToPad)
}
