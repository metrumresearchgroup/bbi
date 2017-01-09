package runner

import (
	"fmt"
	"runtime"
	"testing"
)

func TestPrepareForExecution(t *testing.T) {
	var pathtests []struct {
		in  []string
		out []string
	}
	if runtime.GOOS == "windows" {
		pathtests = []struct {
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
					"$DATA ..\\..\\modeling.csv",
					"$PK",
				},
			},
			{
				[]string{
					"$PROB",
					"$DATA ..\\modeling.csv IGNORE=@",
					"$PK",
				},
				[]string{
					"$PROB",
					"$DATA ..\\..\\modeling.csv IGNORE=@",
					"$PK",
				},
			},
		}
	} else {
		pathtests = []struct {
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
			{
				[]string{
					"$PROB",
					"$DATA ../modeling.csv IGNORE=@",
					"$PK",
				},
				[]string{
					"$PROB",
					"$DATA ../../modeling.csv IGNORE=@",
					"$PK",
				},
			},
		}
	}
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
