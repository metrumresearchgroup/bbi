// Copyright 2026 Metrum Research Group
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package fmatch

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
)

// Rule represents a condition for matching a file name.
type Rule struct {
	Pattern   string // pattern to match against base name
	Glob      bool   // use path/filepath.Match rather than exact match?
	OnlyEmpty bool   // consider a match only if file is empty
	Level     int    // consider a match only if this is at or below caller-specified level
}

// Rules represents a set of conditions for matching a file name.
type Rules struct {
	fixed     map[string]int  // maps fixed pattern to its level
	globs     map[string]int  // maps glob pattern to its level
	onlyempty map[string]bool // whether pattern should match only if file is empty
}

// NewRules returns a Rules instance that captures each Rule in rs.
func NewRules(rs []Rule) Rules {
	fixed := make(map[string]int)
	globs := make(map[string]int)
	onlyempty := make(map[string]bool, len(rs))

	for _, r := range rs {
		l := r.Level
		if l < 1 {
			panic(fmt.Sprintf("bug: pattern %+v has level below 1", r))
		}

		if r.Glob {
			globs[r.Pattern] = l
		} else {
			fixed[r.Pattern] = l
		}
		onlyempty[r.Pattern] = r.OnlyEmpty
	}

	return Rules{fixed: fixed, globs: globs, onlyempty: onlyempty}
}

// Match returns whether the file described by fi matches a rule at or below
// level.  If there is a match, the matching rule's level is returned.
// Otherwise 0 is returned.
//
// If the file name is found in the fixed-pattern rule set, the glob patterns
// are not tested, even if the fixed-pattern rule does not lead to a match due
// to the level or "only empty" criteria.
func (rs Rules) Match(fi fs.FileInfo, level int) (int, error) {
	fname := fi.Name()

	l, found := rs.fixed[fname]
	if found {
		if l <= level && (!rs.onlyempty[fname] || fi.Size() == 0) {
			return l, nil
		}

		return 0, nil
	}

	for glob, l := range rs.globs {
		if l > level {
			continue
		}

		if rs.onlyempty[glob] && fi.Size() > 0 {
			continue
		}

		matched, err := filepath.Match(glob, fname)
		if err != nil {
			return 0, fmt.Errorf("bug: bad pattern: %w", err)
		}

		if matched {
			return l, nil
		}
	}

	return 0, nil
}

// Func is the type of the function called by Rules.Do when a directory entry is
// matched by a rule.  path has the dir argument that's passed to Do as the
// prefix.  level is the level of the pattern that matched.
type Func func(path string, d fs.DirEntry, level int) error

// Do calls function fn for each entry in dir that is matched by rs.Match.
func (rs Rules) Do(dir string, level int, fn Func) error {
	return rs.do(os.DirFS(dir), dir, level, fn)
}

func (rs Rules) do(fsys fs.FS, dir string, level int, fn Func) error {
	entries, err := fs.ReadDir(fsys, ".")
	if err != nil {
		return err
	}

	for _, entry := range entries {
		fi, err := entry.Info()
		if err != nil {
			if os.IsNotExist(err) {
				continue
			}

			return err
		}

		m, err := rs.Match(fi, level)
		if err != nil {
			return err
		}
		if m <= 0 {
			continue
		}

		err = fn(filepath.Join(dir, entry.Name()), entry, m)
		if err != nil {
			return fmt.Errorf("error calling match function: %w", err)
		}
	}

	return nil
}

func getPatterns(m map[string]int, level int) []string {
	xs := make([]string, 0, len(m))
	for k, l := range m {
		if l <= level {
			xs = append(xs, k)
		}
	}
	slices.Sort(xs)

	return xs
}

// FixedPatterns returns the rs patterns for which Glob is false and the rule's
// level is at or below level.
func (rs Rules) FixedPatterns(level int) []string {
	return getPatterns(rs.fixed, level)
}

// GlobPatterns returns the rs patterns for which Glob is true and the rule's
// level is at or below level.
func (rs Rules) GlobPatterns(level int) []string {
	return getPatterns(rs.globs, level)
}

// Patterns returns all of rs patterns for which the rule's level is at or below
// level.
func (rs Rules) Patterns(level int) []string {
	xs := make([]string, 0, len(rs.fixed)+len(rs.globs))
	xs = append(xs, rs.FixedPatterns(level)...)
	xs = append(xs, rs.GlobPatterns(level)...)
	slices.Sort(xs)

	return xs
}
