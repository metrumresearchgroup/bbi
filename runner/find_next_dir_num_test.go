package runner

import (
	"fmt"
	"testing"
)

var nextModelDirTests = []struct {
	in  []string
	out NextDirSuggestion
}{
	{
		[]string{},
		NextDirSuggestion{
			"run001_est_01",
			false,
		},
	},
	{
		[]string{
			"run001_est_01",
			"run001_est_02",
		},
		NextDirSuggestion{
			"run001_est_03",
			false,
		},
	},
	{
		[]string{
			"run001_est_01",
			"run001_est_05",
		},
		NextDirSuggestion{
			"run001_est_06",
			true,
		},
	},
}

func TestFindNextEstDir(t *testing.T) {
	for _, tt := range nextModelDirTests {
		data := FindNextEstDirNum("run001", tt.in, 2)
		if data.NextDirName != tt.out.NextDirName {
			t.Log(fmt.Sprintf("GOT: %s, EXPECTED: %s", data.NextDirName, tt.out.NextDirName))
			t.Fail()
		}
	}
}
