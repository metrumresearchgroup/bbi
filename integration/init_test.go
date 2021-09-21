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
	tests := []struct {
		name string
	}{
		{name: "240"},
		{name: "acop"},
		{name: "ctl_test"},
		{name: "metrum_std"},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			scenario := InitializeScenario(t, test.name)
			scenario.Prepare(t, context.Background())

			t.Run(fmt.Sprintf("init_%s", scenario.identifier), func(t *wrapt.T) {
				_, err := executeCommand(context.Background(), "bbi", "init", "--dir", os.Getenv("NONMEMROOT"))
				t.R.NoError(err)

				t.A.FileExists(filepath.Join(scenario.Workpath, "bbi.yaml"))

				// Verify that we have nonmem contents!
				c := configlib.Config{}

				configHandle, err := os.Open(filepath.Join(scenario.Workpath, "bbi.yaml"))
				t.R.NoError(err)
				defer func() { t.R.NoError(configHandle.Close()) }()

				bytes, err := ioutil.ReadAll(configHandle)
				t.R.NoError(err)

				t.R.NoError(yaml.Unmarshal(bytes, &c))

				t.A.Greater(len(c.Nonmem), 0)
			})
		})
	}
}
