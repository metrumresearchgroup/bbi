package nmless

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/wrapt"

	parser "github.com/metrumresearchgroup/bbi/parsers/nmparser"
)

type jsonResult struct {
	RunName string
	Ok      bool
	Summary parser.ModelInfo
}

func setupMods(t *wrapt.T) string {
	t.Helper()
	dir := t.TempDir()

	fooMod := []byte(`
$PROB foo prob
$SUBROUTINES ADVAN2 TRANS2x
$EST METHOD=1 INTERACTION MAXEVAL=9999
$EST METHOD=SAEM NBURN=3000 NITER=2000
$TABLE ID TIME
$TABLE ID CL
$COV PRINT = E UNCONDITIONAL
`)
	err := os.WriteFile(filepath.Join(dir, "foo.mod"), fooMod, 0o666)
	if err != nil {
		t.Fatal(err)
	}

	// Same but with ctl extension.
	err = os.WriteFile(filepath.Join(dir, "foo.ctl"),
		bytes.Replace(fooMod, []byte("foo prob"), []byte("foo ctl prob"), 1),
		0o666)
	if err != nil {
		t.Fatal(err)
	}

	barMod := []byte(`
$PROBLEM bar prob
$SIM ONLYSIM (393) SUBPROBLEMS=2
`)
	err = os.WriteFile(filepath.Join(dir, "bar.mod"), barMod, 0o666)
	if err != nil {
		t.Fatal(err)
	}

	return dir
}

func TestProbs(tt *testing.T) {
	t := wrapt.WrapT(tt)
	dir := setupMods(t)

	t.Run("table output", func(t *wrapt.T) {
		cmd := exec.Command("bbi", "nonmem", "probs")
		cmd.Dir = dir

		bs, err := cmd.Output()
		if err != nil {
			t.Fatal(err)
		}
		out := string(bs)

		t.A.Contains(out, "---")
		t.A.Contains(out, "foo prob")
		// FIXME: .mod extension is considered but not .ctl.
		t.A.NotContains(out, "foo ctl prob")
		// FIXME: ParseModInfo assumes short "$PROB".
		t.A.Contains(out, "LEM bar prob")
	})

	t.Run("json output", func(t *wrapt.T) {
		cmd := exec.Command("bbi", "nonmem", "probs", "--json")
		cmd.Dir = dir

		bs, err := cmd.Output()
		if err != nil {
			t.Fatal(err)
		}

		var recs []jsonResult
		err = json.Unmarshal(bs, &recs)
		if err != nil {
			t.Fatal(err)
		}

		t.A.Len(recs, 2)

		for _, rec := range recs {
			switch rec.RunName {
			case "foo":
				t.A.Len(rec.Summary.Tables, 2)
				t.A.Equal(rec.Summary.Est, []parser.Est{
					{
						Method: "$EST METHOD=1 INTERACTION MAXEVAL=9999",
					},
					{
						Method: "$EST METHOD=SAEM NBURN=3000 NITER=2000",
					},
				})
				t.A.True(rec.Summary.Cov.Ok)
				t.A.False(rec.Summary.Sim.Ok)
			case "bar":
				t.A.Len(rec.Summary.Tables, 0)
				t.A.Len(rec.Summary.Est, 0)
				t.A.False(rec.Summary.Cov.Ok)
				t.A.True(rec.Summary.Sim.Ok)
			default:
				t.Errorf("expected foo or bar for RunName, got %s", rec.RunName)
			}
		}
	})
}

func TestProbsErrors(tt *testing.T) {
	t := wrapt.WrapT(tt)

	cmd := exec.Command("bbi", "nonmem", "probs", "one", "two")
	_, err := cmd.Output()
	t.R.Error(err)

	var exitError *exec.ExitError
	if ok := errors.As(err, &exitError); !ok {
		t.Fatalf("got error %v; want ExitError", err)
	}

	t.A.Contains(string(exitError.Stderr), "one directory")
}
