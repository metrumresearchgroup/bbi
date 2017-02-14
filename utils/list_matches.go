package utils

import (
	"errors"

	"regexp"
)

// ListMatchesByRegex returns all matches by a regex
// as regexp.MustCompile panics if the regex does not compile, the function
// is designed to recover and return an error immediately
func ListMatchesByRegex(names []string, regex string) (matches []string, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = errors.New("could not parse regex")
		}
	}()
	r := regexp.MustCompile(regex)
	for _, n := range names {
		if r.MatchString(n) {
			matches = append(matches, n)
		}
	}
	return
}

// ListNonMatchesByRegex returns all matches by a regex
// as regexp.MustCompile panics if the regex does not compile, the function
// is designed to recover and return an error immediately
func ListNonMatchesByRegex(names []string, regex string) (nonmatches []string, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = errors.New("could not parse regex")
		}
	}()
	r := regexp.MustCompile(regex)
	for _, n := range names {
		if !r.MatchString(n) {
			nonmatches = append(nonmatches, n)
		}
	}
	return
}
