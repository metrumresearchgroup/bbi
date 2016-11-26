package runner

import (
	"fmt"
	"testing"
)

var pathtests = []struct {
	in  []string
	out []string
}{
	{
		[]string{
			"$PROB",
			"$DATA ../modeling.csv",
			"$PK",
		},
		[]string{
			"$PROB",
			"$DATA ../../modeling.csv",
			"$PK",
		},
	},
}

func TestPrepareForExecution(t *testing.T) {
	for _, tt := range pathtests {
		res := PrepareForExecution(tt.in)
		for i, l := range res {
			if l != tt.out[i] {
				t.Log(fmt.Sprintf(", GOT: %s, EXPECTED: %s, at INDEX: %b", tt.in, tt.out, i))
				t.Fail()
			}
		}
	}
}
