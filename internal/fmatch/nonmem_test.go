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
	"io/fs"
	"os"
	"slices"
	"strconv"
	"testing"
	"testing/fstest"

	"github.com/metrumresearchgroup/bbi/configlib"
)

func TestNonmemCleanMatcher(t *testing.T) {
	rootdir := "somedir"

	tests := []struct {
		fsys     fstest.MapFS
		level    int
		parallel bool
		want     []string
	}{
		{
			fsys:  fstest.MapFS{"1.lst": {}},
			level: 1,
			want:  []string{},
		},
		{
			fsys: fstest.MapFS{
				"1.lst":    {},
				"FDATA":    {},
				"temp_dir": {Mode: 0o755 | os.ModeDir},
			},
			level: 1,
			want: []string{
				rootdir + "/FDATA",
				rootdir + "/temp_dir",
			},
		},
		{
			fsys: fstest.MapFS{
				"1.lst":    {},
				"FDATA":    {},
				"temp_dir": {Mode: 0o755 | os.ModeDir},
			},
			level: 0,
			want:  []string{},
		},
		{
			fsys: fstest.MapFS{
				"1.lst":     {},
				"FDATA":     {},
				"temp_dir":  {Mode: 0o755 | os.ModeDir},
				"WK_12_foo": {},
			},
			level: 1,
			want: []string{
				rootdir + "/FDATA",
				rootdir + "/temp_dir",
			},
		},
		{
			fsys: fstest.MapFS{
				"1.lst":       {},
				"FDATA":       {},
				"temp_dir":    {Mode: 0o755 | os.ModeDir},
				"WK_12_foo":   {},
				"WK_12_bar":   {Data: []byte("not empty")},
				"fort11blah":  {},
				"fort.13blah": {},
				"worker3":     {},
			},
			level:    1,
			parallel: true,
			want: []string{
				rootdir + "/FDATA",
				rootdir + "/WK_12_foo",
				rootdir + "/fort.13blah",
				rootdir + "/temp_dir",
				rootdir + "/worker3",
			},
		},
	}

	for i, tt := range tests {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			rs := NonmemCleanMatcher(configlib.Config{Parallel: tt.parallel})
			got := []string{}
			err := rs.do(tt.fsys, rootdir, tt.level,
				func(p string, _ fs.DirEntry, level int) error {
					got = append(got, p)

					return nil
				},
			)
			if err != nil {
				t.Fatal(err)
			}

			if !slices.Equal(got, tt.want) {
				t.Errorf("\ngot  %+v\nwant %+v", got, tt.want)
			}
		})
	}
}
