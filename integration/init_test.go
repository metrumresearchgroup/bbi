package bbitest

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
	"gopkg.in/yaml.v2"

	"github.com/metrumresearchgroup/bbi/configlib"
)

func TestInitialization(tt *testing.T) {
	t := wrapt.WrapT(tt)

	scenarios, err := InitializeScenarios([]string{
		"240",
		"acop",
		"ctl_test",
		"metrum_std",
	})
	t.R.NoError(err)
	t.R.Len(scenarios, 4)

	for _, s := range scenarios {
		tt.Run(s.identifier, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			err = s.Prepare(context.Background())
			t.R.NoError(err)

			tt.Run(fmt.Sprintf("init_%s", s.identifier), func(tt *testing.T) {
				t := wrapt.WrapT(tt)

				_, err = executeCommand(context.Background(), "bbi", "init", "--dir", os.Getenv("NONMEMROOT"))

				t.A.Nil(err)

				t.A.FileExists(filepath.Join(s.Workpath, "bbi.yaml"))

				// Verify that we have nonmem contents!
				c := configlib.Config{}

				configHandle, _ := os.Open(filepath.Join(s.Workpath, "bbi.yaml"))
				bytes, _ := ioutil.ReadAll(configHandle)
				err = yaml.Unmarshal(bytes, &c)
				t.R.NoError(err)

				t.A.Greater(len(c.Nonmem), 0)
				err = configHandle.Close()
				t.R.NoError(err)
			})
		})
	}
}
