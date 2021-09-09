package runner

import (
	"fmt"
	"testing"
)

// TODO: remove initResults if we find no use
/* func initResults() map[string]int {
	var results01 = make(map[string]int)
	results01["run001.clt"] = 1
	results01["run001.coi"] = 1
	results01["run001.cor"] = 1
	results01["run001.cov"] = 1
	results01["run001.cpu"] = 1
	results01["run001.ext"] = 1
	results01["run001.grd"] = 1
	results01["run001.lst"] = 1
	results01["run001.phi"] = 1
	results01["run001.shk"] = 1
	results01["run001.shm"] = 1
	results01["run001.xml"] = 1

	return results01
} */

func initFilenameKeys() []string {
	return []string{
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
	}
}

var estOutputFilesByRunTests = []struct {
	in  string
	out []string
}{
	{
		"run001",
		initFilenameKeys(),
	},
}

func TestEstOutputFilesByRun(t *testing.T) {
	for _, tt := range estOutputFilesByRunTests {
		data := EstOutputFilesByRun(tt.in)
		for _, key := range tt.out {
			_, ok := data[key]
			fmt.Println(ok)
			if !ok {
				t.Log(fmt.Sprintf("GOT: NO KEY, EXPECTED: %s", key))
				t.Fail()
			}
		}
	}
}
