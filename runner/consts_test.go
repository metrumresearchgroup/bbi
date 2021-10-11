package runner

import (
	"testing"

	"github.com/metrumresearchgroup/wrapt"

	"github.com/metrumresearchgroup/bbi/utils"
)

func TestEstOutputFilesByRun(tt *testing.T) {
	var tests = []struct {
		in  string
		out []string
	}{
		{
			"run001",
			[]string{
				"run001.clt",
				"run001.coi",
				"run001.cor",
				"run001.cov",
				"run001.cpu",
				"run001.ext",
				"run001.grd",
				"run001.lst",
				"run001.phi",
				"run001.shk",
				"run001.shm",
				"run001.xml",
			},
		},
	}

	testId := "UNIT-RUN-001"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.in, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got := EstOutputFilesByRun(test.in)

			for _, key := range test.out {
				_, ok := got[key]

				t.A.True(ok, "missing key %s", key)
			}
		})
	}
}
