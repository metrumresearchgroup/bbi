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

package parser

import (
	"bytes"
	"path/filepath"
	"strconv"
	"testing"
)

func TestDataFile(t *testing.T) {
	tests := []struct {
		input []byte
		want  string
	}{
		{
			input: []byte("$DATA x"),
			want:  "x",
		},
		{
			input: []byte("$DATA xy"),
			want:  "xy",
		},
		{
			input: []byte("$daT x"),
			want:  "x",
		},
		{
			input: []byte(`; c
$PROB foo $DATA
; $DATA a
$DATA   b
; $DATA c
`),
			want: "b",
		},
		{
			input: []byte(`$DATA "one"`),
			want:  "one",
		},
		{
			input: []byte("$DATA 'one'"),
			want:  "one",
		},
		{
			input: []byte(`$DATA 'one'
`),
			want: "one",
		},
		{
			input: []byte("$DATA 'one;two'"),
			want:  "one;two",
		},
		{
			input: []byte(`
$PROB prob
  	
; c 
$DATA fname rest`),
			want: "fname",
		},
		{
			input: []byte(`
; c
$DATA fname`),
			want: "fname",
		},
		{
			input: []byte(`
$DATA fname
$DATA IGNORE=c1`),
			want: "fname",
		},
		{
			// bbi does not support defining multiple problems (gh-187).  If it
			// is encountered, the first file is returned.
			input: []byte(`
$PROB p1
$DATA fname1
$PROB p2
$DATA fname2`),
			want: "fname1",
		},
		{
			input: []byte(`
$PROB p1
$DATA fname
$PROB p2
$DATA *`),
			want: "fname",
		},
	}

	for i, tt := range tests {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			got, err := DataFile(tt.input)
			if err != nil {
				t.Fatal(err)
			}

			if got != tt.want {
				t.Errorf("filename: got %s, want %s", got, tt.want)
			}
		})
	}
}

func TestDataFileErrors(t *testing.T) {
	tests := []struct {
		input []byte
	}{
		{
			input: []byte{},
		},
		{
			input: []byte("foo"),
		},
		{
			input: []byte("$PROBLEM p"),
		},
		{
			input: []byte(`
$PROB p
; $DATA foo`),
		},
		{
			input: []byte("$DATA"),
		},
		{
			input: []byte("$DATA "),
		},
		{
			input: []byte("$DATAx"),
		},
		{
			input: []byte("; $DATA xy"),
		},
		{
			input: []byte("  ; $DATA xy"),
		},
		{
			input: []byte("$DATA ''"),
		},
		{
			input: []byte("$DATA '"),
		},
		{
			input: []byte("$DATA 'one"),
		},
		{
			input: []byte(`$DATA 'one"`),
		},
		{
			input: []byte(`$DATA 'one
'`),
		},
		{
			input: []byte(`$DATA "`),
		},
		{
			input: []byte("$DATA ;"),
		},
	}

	for i, tt := range tests {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			f, err := DataFile(tt.input)
			if err == nil {
				t.Errorf("got %q, want error", f)
			}
		})
	}
}

func TestAdjustDataFile(t *testing.T) {
	tests := []struct {
		input []byte
		want  []byte
	}{
		{
			input: []byte("$DATA x"),
			want:  []byte("$DATA " + filepath.Join("..", "x")),
		},
		{
			input: []byte("$DATA 'xyz'"),
			want:  []byte("$DATA '" + filepath.Join("..", "xyz") + "'"),
		},
		{
			input: []byte(`$DATA "xyz"`),
			want:  []byte(`$DATA "` + filepath.Join("..", "xyz") + `"`),
		},
		{
			input: []byte(`
$PROB p
;$DATA foo
$DATA   ../foo/bar.csv
;$DATA foo
`),
			want: []byte(`
$PROB p
;$DATA foo
$DATA   ` + filepath.Join("..", "../foo/bar.csv") + "\n;$DATA foo\n"),
		},
		{
			input: []byte("$DATA /abs/file"),
			want:  []byte("$DATA /abs/file"),
		},
		{
			input: []byte("$DATA '/abs/file'"),
			want:  []byte("$DATA '/abs/file'"),
		},
		{
			input: []byte(`
$DATA fname
$DATA IGNORE=c1`),
			want: []byte(`
$DATA ` + filepath.Join("..", "fname") + "\n$DATA IGNORE=c1"),
		},
		{
			input: []byte(`
$DATA fname1
$DATA fname2`),
			want: []byte(`
$DATA ` + filepath.Join("..", "fname1") + "\n$DATA fname2"),
		},
	}

	for i, tt := range tests {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			got, err := AdjustDataFile(tt.input)
			if err != nil {
				t.Fatal(err)
			}

			if !bytes.Equal(got, tt.want) {
				t.Errorf("content\n got: %q\nwant: %q", got, tt.want)
			}
		})
	}
}

func TestAdjustDataFileErrors(t *testing.T) {
	tests := []struct {
		input []byte
	}{
		{
			input: []byte{},
		},
		{
			input: []byte("$PROB p"),
		},
		{
			input: []byte(`
$PROB p
;$DATA foo
`),
		},
	}

	for i, tt := range tests {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			got, err := AdjustDataFile(tt.input)
			if err == nil {
				t.Errorf("got %q, want error", got)
			}
		})
	}
}
