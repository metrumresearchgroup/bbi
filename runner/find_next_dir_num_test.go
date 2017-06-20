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
			true,
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
			false,
		},
	},
}

func TestFindNextEstDir(t *testing.T) {
	for i, tt := range nextModelDirTests {
		data := FindNextEstDirNum("run001", tt.in, 2)
		if data.NextDirName != tt.out.NextDirName {
			t.Log(fmt.Sprintf("test %v, Incorrect NextDirName: GOT: %s, EXPECTED: %s", i, data.NextDirName, tt.out.NextDirName))
			t.Fail()
		}
		if data.FirstRun != tt.out.FirstRun {
			t.Log(fmt.Sprintf("test %v, Incorrect FirstRun: GOT: %v, EXPECTED: %v", i, data.FirstRun, tt.out.FirstRun))
			t.Fail()
		}
		if data.Reorg != tt.out.Reorg {
			t.Log(fmt.Sprintf("test %v Incorrect Reorg: GOT: %v, EXPECTED: %v", i, data.Reorg, tt.out.Reorg))
			t.Fail()
		}
	}
}
