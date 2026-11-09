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
	"errors"
	"io/fs"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"testing"
	"testing/fstest"
)

func TestPatterns(t *testing.T) {
	tests := []struct {
		rules        []Rule
		level        int
		wantFixed    []string
		wantGlobs    []string
		wantPatterns []string
	}{
		{
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   1,
				},
			},
			level:        1,
			wantFixed:    []string{"foo"},
			wantGlobs:    []string{},
			wantPatterns: []string{"foo"},
		},
		{
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   1,
				},
				{
					Pattern: "bar",
					Glob:    true,
					Level:   1,
				},
				{
					Pattern: "baz",
					Level:   1,
				},
			},
			level:        1,
			wantFixed:    []string{"baz", "foo"},
			wantGlobs:    []string{"bar"},
			wantPatterns: []string{"bar", "baz", "foo"},
		},
		{
			rules: []Rule{
				{
					Pattern: "a",
					Level:   2,
				},
				{
					Pattern: "b",
					Glob:    true,
					Level:   2,
				},
				{
					Pattern: "c",
					Glob:    true,
					Level:   1,
				},
				{
					Pattern: "d",
					Level:   1,
				},
			},
			level:        1,
			wantFixed:    []string{"d"},
			wantGlobs:    []string{"c"},
			wantPatterns: []string{"c", "d"},
		},
		{
			rules: []Rule{
				{
					Pattern: "a",
					Level:   2,
				},
				{
					Pattern: "b",
					Glob:    true,
					Level:   3,
				},
				{
					Pattern: "c",
					Glob:    true,
					Level:   1,
				},
				{
					Pattern: "d",
					Level:   1,
				},
			},
			level:        2,
			wantFixed:    []string{"a", "d"},
			wantGlobs:    []string{"c"},
			wantPatterns: []string{"a", "c", "d"},
		},
	}

	for i, tt := range tests {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			rs := NewRules(tt.rules)

			got := rs.FixedPatterns(tt.level)
			if !slices.Equal(got, tt.wantFixed) {
				t.Errorf("fixed: got %v, want %v", got, tt.wantFixed)
			}

			got = rs.GlobPatterns(tt.level)
			if !slices.Equal(got, tt.wantGlobs) {
				t.Errorf("globs: got %v, want %v", got, tt.wantGlobs)
			}

			got = rs.Patterns(tt.level)
			if !slices.Equal(got, tt.wantPatterns) {
				t.Errorf("patterns: got %v, want %v", got, tt.wantPatterns)
			}
		})
	}
}

func TestMatch(t *testing.T) {
	tests := []struct {
		rules []Rule
		fsys  fstest.MapFS
		name  string
		level int
		want  int
	}{
		{
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   1,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			name:  "foo",
			level: 1,
			want:  1,
		},
		{
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   1,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			name:  "foo",
			level: 2,
			want:  1,
		},
		{
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   2,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			name:  "foo",
			level: 1,
			want:  0,
		},
		{
			// Glob is not considered if there is an exact match.
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   2,
				},
				{
					Pattern: "fo*",
					Glob:    true,
					Level:   1,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			name:  "foo",
			level: 1,
			want:  0,
		},
		{
			rules: []Rule{
				{
					Pattern: "fo*",
					Glob:    true,
					Level:   1,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			name:  "foo",
			level: 1,
			want:  1,
		},
		{
			rules: []Rule{
				{
					Pattern: "f*",
					Glob:    true,
					Level:   2,
				},
				{
					Pattern: "fo*",
					Glob:    true,
					Level:   1,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			name:  "foo",
			level: 1,
			want:  1,
		},
		{
			rules: []Rule{
				{
					Pattern:   "foo",
					OnlyEmpty: true,
					Level:     1,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			name:  "foo",
			level: 1,
			want:  1,
		},
		{
			rules: []Rule{
				{
					Pattern:   "foo",
					OnlyEmpty: true,
					Level:     1,
				},
			},
			fsys:  fstest.MapFS{"foo": {Data: []byte("not empty")}},
			name:  "foo",
			level: 1,
			want:  0,
		},
		{
			rules: []Rule{
				{
					Pattern:   "fo?",
					Glob:      true,
					OnlyEmpty: true,
					Level:     1,
				},
			},
			fsys:  fstest.MapFS{"foo": {Data: []byte("not empty")}},
			name:  "foo",
			level: 1,
			want:  0,
		},
		{
			rules: []Rule{
				{
					Pattern:   "fo?",
					Glob:      true,
					OnlyEmpty: true,
					Level:     1,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			name:  "foo",
			level: 1,
			want:  1,
		},
		{
			rules: []Rule{
				{
					Pattern:   "fo?",
					Glob:      true,
					OnlyEmpty: true,
					Level:     1,
				},
				{
					Pattern:   "fo?",
					Glob:      true,
					OnlyEmpty: false,
					Level:     2,
				},
			},
			fsys:  fstest.MapFS{"foo": {Data: []byte("not empty")}},
			name:  "foo",
			level: 3,
			want:  2,
		},
	}

	for i, tt := range tests {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			rs := NewRules(tt.rules)

			fi, err := tt.fsys.Stat(tt.name)
			if err != nil {
				t.Fatal(err)
			}

			got, err := rs.Match(fi, tt.level)
			if err != nil {
				t.Fatal(err)
			}

			if got != tt.want {
				t.Errorf("match: got %d, want %d", got, tt.want)
			}
		})
	}
}

func TestDo(t *testing.T) {
	type Hit struct {
		path  string
		level int
	}

	rootdir := "somedir"

	tests := []struct {
		rules []Rule
		fsys  fstest.MapFS
		level int
		want  []Hit
	}{
		{
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   2,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			level: 1,
			want:  []Hit{},
		},
		{
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   1,
				},
			},
			fsys:  fstest.MapFS{"foo": {}},
			level: 1,
			want: []Hit{
				{
					path:  rootdir + "/foo",
					level: 1,
				},
			},
		},
		{
			rules: []Rule{
				{
					Pattern: "ba?",
					Glob:    true,
					Level:   1,
				},
			},
			fsys: fstest.MapFS{
				"foo": {},
				"bar": {},
				"baz": {},
			},
			level: 2,
			want: []Hit{
				{
					path:  rootdir + "/bar",
					level: 1,
				},
				{
					path:  rootdir + "/baz",
					level: 1,
				},
			},
		},
		{
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   2,
				},
				{
					Pattern: "ba?",
					Glob:    true,
					Level:   1,
				},
			},
			fsys: fstest.MapFS{
				"foo":    {},
				"foobar": {},
				"bar":    {},
				"baz":    {},
			},
			level: 1,
			want: []Hit{
				{
					path:  rootdir + "/bar",
					level: 1,
				},
				{
					path:  rootdir + "/baz",
					level: 1,
				},
			},
		},
		{
			rules: []Rule{
				{
					Pattern: "foo",
					Level:   2,
				},
				{
					Pattern:   "ba?",
					Glob:      true,
					OnlyEmpty: true,
					Level:     1,
				},
			},
			fsys: fstest.MapFS{
				"foo":    {},
				"foobar": {},
				"baa":    {Data: []byte("not empty")},
				"bar":    {},
				"baz":    {},
			},
			level: 2,
			want: []Hit{
				{
					path:  rootdir + "/bar",
					level: 1,
				},
				{
					path:  rootdir + "/baz",
					level: 1,
				},
				{
					path:  rootdir + "/foo",
					level: 2,
				},
			},
		},
	}

	for i, tt := range tests {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			rs := NewRules(tt.rules)

			got := []Hit{}
			err := rs.do(tt.fsys, rootdir, tt.level,
				func(p string, _ fs.DirEntry, level int) error {
					got = append(got, Hit{path: p, level: level})

					return nil
				},
			)
			if err != nil {
				t.Fatal(err)
			}

			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("\ngot  %+v\nwant %+v", got, tt.want)
			}
		})
	}
}

func TestDoError(t *testing.T) {
	rootdir := "somedir"
	fsys := fstest.MapFS{
		"foo": {},
	}

	rules := []Rule{
		{
			Pattern: "foo",
			Level:   1,
		},
	}
	rs := NewRules(rules)

	err := rs.do(fsys, rootdir, 1,
		func(p string, _ fs.DirEntry, level int) error {
			return errors.New("fnerror")
		},
	)
	if err == nil {
		t.Fatal("want error")
	}

	if want := "fnerror"; !strings.Contains(err.Error(), want) {
		t.Errorf("got %q, want error with %q", err.Error(), want)
	}
}
