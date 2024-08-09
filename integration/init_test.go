package bbitest

import (
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/wrapt"
	"gopkg.in/yaml.v2"

	"github.com/metrumresearchgroup/bbi/configlib"
)

func TestInitialization(tt *testing.T) {
	tests := []struct {
		name string
	}{
		{name: "acop"},
	}

	testId := "INT-INIT-001"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
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

				bytes, err := io.ReadAll(configHandle)
				t.R.NoError(err)

				t.R.NotContains(bytes, []byte("bbi_binary"))
				t.R.NoError(yaml.Unmarshal(bytes, &c))
				t.A.Equal(c.BbiBinary, "")
				t.A.Greater(len(c.Nonmem), 0)
				t.A.Equal(2, c.NMFEOptions.MaxLim)
			})
		})
	}
}

func TestInitializationSanitization(tt *testing.T) {
	id := "INT-INIT-002"
	tt.Run(utils.AddTestId("", id), func(tt *testing.T) {
		t := wrapt.WrapT(tt)
		tdir := t.TempDir()
		dir := filepath.Join(tdir, "x.y")
		for _, d := range []string{"license", "run", "util", "source"} {
			sdir := filepath.Join(dir, d)
			if err := os.MkdirAll(sdir, 0777); err != nil {
				t.Fatal(err)
			}
		}

		lic := filepath.Join(dir, "license", "nonmem.lic")
		if err := os.WriteFile(lic, []byte(""), 0666); err != nil {
			t.Fatal(err)
		}

		nmfe := filepath.Join(dir, "run", "nmfe12")
		if runtime.GOOS == "windows" {
			nmfe = nmfe + ".bat"
		}

		if err := os.WriteFile(nmfe, []byte(""), 0777); err != nil {
			t.Fatal(err)
		}

		cmd := exec.Command("bbi", "init", "--dir", tdir)
		cmd.Dir = tdir
		t.R.NoError(cmd.Run())

		c := configlib.Config{}
		fh, err := os.Open(filepath.Join(tdir, "bbi.yaml"))
		t.R.NoError(err)
		defer fh.Close()

		bytes, err := io.ReadAll(fh)
		t.R.NoError(err)
		t.R.NoError(yaml.Unmarshal(bytes, &c))
		t.R.Equal(c.Nonmem["x-y"].Home, filepath.Join(tdir, "x.y"))
	})
}
