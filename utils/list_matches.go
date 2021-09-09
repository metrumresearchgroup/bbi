package utils

// ListMatchesByRegex returns all matches by a regex
// as regexp.MustCompile panics if the regex does not compile, the function
// is designed to recover and return an error immediately.
func ListMatchesByRegex(names []string, regex StringMatcher) []string {
	matches := make([]string, 0, len(names))
	for _, n := range names {
		if regex.MatchString(n) {
			matches = append(matches, n)
		}
	}

	return matches
}

// ListNonMatchesByRegex returns all matches by a regex
// as regexp.MustCompile panics if the regex does not compile, the function
// is designed to recover and return an error immediately.
func ListNonMatchesByRegex(names []string, regex StringMatcher) []string {
	nonmatches := make([]string, 0, len(names))
	for _, n := range names {
		if !regex.MatchString(n) {
			nonmatches = append(nonmatches, n)
		}
	}

	return nonmatches
}

// ListMatchesByGlob returns all matches by a glob
// as glob.MustCompile panics if the glob does not compile, the function
// is designed to recover and return an error immediately.
func ListMatchesByGlob(names []string, gb Matcher) []string {
	matches := make([]string, 0, len(names))
	for _, n := range names {
		if gb.Match(n) {
			matches = append(matches, n)
		}
	}

	return matches
}

// ListNonMatchesByGlob returns all matches by a glob
// as glob.MustCompile panics if the glob does not compile, the function
// is designed to recover and return an error immediately.
func ListNonMatchesByGlob(names []string, gb Matcher) []string {
	nonmatches := make([]string, 0, len(names))
	for _, n := range names {
		if !gb.Match(n) {
			nonmatches = append(nonmatches, n)
		}
	}

	return nonmatches
}

type StringMatcher interface {
	MatchString(string) bool
}

type Matcher interface {
	Match(string) bool
}
