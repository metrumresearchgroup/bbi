package parser

import (
	"fmt"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestOmega(tt *testing.T) {
	want := omegaIndices
	got := omega(50)

	testId := "UNIT-NMP-014"
	tt.Run(testId, func(tt *testing.T) {
		t := wrapt.WrapT(tt)

		t.R.Equal(1275, len(got))

		toInts := func(v [2]int) (int, int) {
			return v[0], v[1]
		}

		// convert to a map to check against the hand-coded table
		asMap := make(map[int]string, len(got))

		for i := 0; i < len(got); i++ {
			x, y := toInts(got[i])
			asMap[i] = fmt.Sprintf("(%d,%d)", x, y)
		}

		t.R.Equal(want, asMap)
	})
}
