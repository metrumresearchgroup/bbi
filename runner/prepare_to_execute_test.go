package runner

import (
	"runtime"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestPrepareForExecution(tt *testing.T) {
	type pathtest struct {
		name string
		in   []string
		out  []string
	}

	var tests []pathtest

	if runtime.GOOS == "windows" {
		tests = []pathtest{
			{
				name: "no ignore",
				in: []string{
					"$PROB",
					"$DATA ../modeling.csv",
					"$PK",
				},
				out: []string{
					"$PROB",
					"$DATA ..\\..\\modeling.csv",
					"$PK",
				},
			},
			{
				name: "ignore",
				in: []string{
					"$PROB",
					"$DATA ..\\modeling.csv IGNORE=@",
					"$PK",
				},
				out: []string{
					"$PROB",
					"$DATA ..\\..\\modeling.csv IGNORE=@",
					"$PK",
				},
			},
		}
	} else {
		tests = []pathtest{
			{
				name: "no ignore",
				in: []string{
					"$PROB",
					"$DATA ../modeling.csv",
					"$PK",
				},
				out: []string{
					"$PROB",
					"$DATA ../../modeling.csv",
					"$PK",
				},
			},
			{
				name: "ignore",
				in: []string{
					"$PROB",
					"$DATA ../modeling.csv IGNORE=@",
					"$PK",
				},
				out: []string{
					"$PROB",
					"$DATA ../../modeling.csv IGNORE=@",
					"$PK",
				},
			},
		}
	}
	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			res := PrepareForExecution(test.in)

			t.R.Equal(test.out, res)
		})
	}
}
