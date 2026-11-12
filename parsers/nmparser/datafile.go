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
	"errors"
	"path/filepath"
)

// DataFile returns the filename option of the first $DATA record in s, the
// content of the control stream.
func DataFile(s []byte) (string, error) {
	i, j := boundsDataFile(s)
	if i < 0 || j < 0 {
		return "", errors.New("no $DATA filename found")
	}

	return string(s[i:j]), nil
}

// AdjustDataFile returns a copy of s where the filename option of first $DATA
// record is modified to including a leading ".." path component.  The option is
// not adjusted if it specifies an absolute path.
func AdjustDataFile(s []byte) ([]byte, error) {
	i, j := boundsDataFile(s)
	if i < 0 || j < 0 {
		return nil, errors.New("no $DATA filename found")
	}

	fname := string(s[i:j])
	if filepath.IsAbs(fname) {
		// Not making any changes, but return a copy for consistency.
		return append([]byte(nil), s...), nil
	}

	fname = filepath.Join("..", fname)
	parts := [][]byte{s[0:i], []byte(fname), s[j:]}

	return bytes.Join(parts, []byte{}), nil
}

// boundsDataFile returns the indices of the filename option for the first $DATA
// record in s.  If the option value is surrounded by quotes, the bounds are
// adjusted to exclude the quotes.  Returns -1 for both values if no filename is
// found.
func boundsDataFile(s []byte) (int, int) {
	i := indexDataFile(s)
	if i == -1 {
		return -1, -1
	}

	nbytes := len(s)
	if s[i] == '"' || s[i] == '\'' {
		qchar := s[i]
		i++
		if i == nbytes {
			return -1, -1
		}

		// Note: NM-TRAN doesn't have an escape character, so the next
		// occurrence of qchar terminates the quoted value.
		offset := bytes.IndexByte(s[i:], qchar)
		if offset < 1 {
			return -1, -1
		}

		nl := bytes.IndexByte(s[i:], '\n')
		if nl != -1 && nl < offset {
			return -1, -1
		}

		return i, i + offset
	}

	j := i
	for j < nbytes && !isSpace(s[j]) && s[j] != ';' {
		j++
	}

	if i == j {
		return -1, -1
	}

	return i, j
}

// indexDataFile returns the index of the first option (filename) of the first
// $DATA record in s.  Returns -1 if no $DATA record is found or there isn't an
// option on the same line as $DATA.
func indexDataFile(s []byte) int {
	nbytes := len(s)
	for i := 0; i < nbytes; {
		for i < nbytes && isSpace(s[i]) {
			i++
		}
		if i == nbytes {
			return -1
		}

		if s[i] != '$' {
			for i < nbytes && s[i] != '\n' {
				i++
			}

			continue
		}

		j := i
		for j < nbytes && !isSpace(s[j]) {
			j++
		}

		rec := bytes.ToLower(s[i:j])
		if !bytes.Equal(rec, []byte("$data")) && !bytes.Equal(rec, []byte("$dat")) {
			for i < nbytes && s[i] != '\n' {
				i++
			}

			continue
		}

		i = j
		for i < nbytes && s[i] != '\n' && isSpace(s[i]) {
			i++
		}

		if i < nbytes {
			return i
		}
	}

	return -1
}

func isSpace(c byte) bool {
	if c > 32 {
		return false
	}

	return c == ' ' || c == '\n' || c == '\t' || c == '\v' || c == '\f' || c == '\r'
}
