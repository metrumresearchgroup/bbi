package utils

import (
	"errors"
	"regexp"

	"github.com/gobwas/glob"
)

// ListMatchesByRegex returns all matches by a regex
// as regexp.MustCompile panics if the regex does not compile, the function
// is designed to recover and return an error immediately.
func ListMatchesByRegex(names []string, regex string) (matches []string, err error) {
	r, err := regexp.Compile(regex)
	if err != nil {
		return nil, err
	}

	for _, n := range names {
		if r.MatchString(n) {
			matches = append(matches, n)
		}
	}

	return matches, nil
}

// ListNonMatchesByRegex returns all matches by a regex
// as regexp.MustCompile panics if the regex does not compile, the function
// is designed to recover and return an error immediately.
func ListNonMatchesByRegex(names []string, regex string) (nonmatches []string, err error) {
	r, err := regexp.Compile(regex)
	if err != nil {
		return nil, err
	}
	for _, n := range names {
		if !r.MatchString(n) {
			nonmatches = append(nonmatches, n)
		}
	}

	return nonmatches, nil
}

// ListMatchesByGlob returns all matches by a glob
// as glob.MustCompile panics if the glob does not compile, the function
// is designed to recover and return an error immediately.
func ListMatchesByGlob(names []string, gb string) (matches []string, err error) {
	g, err := glob.Compile(gb)
	if err != nil {
		return nil, err
	}
	for _, n := range names {
		if g.Match(n) {
			matches = append(matches, n)
		}
	}

	return matches, nil
}

// ListNonMatchesByGlob returns all matches by a glob
// as glob.MustCompile panics if the glob does not compile, the function
// is designed to recover and return an error immediately.
func ListNonMatchesByGlob(names []string, gb string) (nonmatches []string, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = errors.New("could not parse glob")
		}
	}()
	g := glob.MustCompile(gb)
	for _, n := range names {
		if !g.Match(n) {
			nonmatches = append(nonmatches, n)
		}
	}

	return nonmatches, nil
}
