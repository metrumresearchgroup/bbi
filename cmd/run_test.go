package cmd

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/metrumresearchgroup/turnstile"
	"github.com/metrumresearchgroup/wrapt"
)

func skipIfNoSh(t *wrapt.T) {
	t.Helper()
	_, err := exec.LookPath("sh")
	if err != nil {
		t.Skipf("skipped because sh is not available")
	}
}

func never(_ error, _ string) bool {
	return false
}

func TestRunModelCommand(tt *testing.T) {
	t := wrapt.WrapT(tt)

	skipIfNoSh(t)

	cmd := exec.Command("sh", "-c", "printf 'stdout\n'; printf >&2 'stderr\n'")
	mod := &NonMemModel{Model: "foo.ctl", FileName: "foo", OutputDir: t.TempDir()}
	cerr := runModelCommand(mod, cmd, never)
	t.A.Equal(cerr, turnstile.ConcurrentError{})

	outfile := filepath.Join(mod.OutputDir, mod.Model+".out")
	t.R.FileExists(outfile)
	bs, err := os.ReadFile(outfile)
	t.R.NoError(err)
	output = string(bs)

	t.A.Contains(output, "stdout")
	t.A.Contains(output, "stderr")
}

func TestRunModelCommandError(tt *testing.T) {
	t := wrapt.WrapT(tt)

	skipIfNoSh(t)

	cmd := exec.Command("sh", "-c", "printf 'stdout\n'; printf >&2 'stderr\n'; exit 1")
	mod := &NonMemModel{Model: "foo.ctl", FileName: "foo", OutputDir: t.TempDir()}

	cerr := runModelCommand(mod, cmd, never)
	t.A.Error(cerr.Error)
	t.A.Contains(cerr.Notes, mod.Model+".out")

	outfile := filepath.Join(mod.OutputDir, mod.Model+".out")
	t.R.FileExists(outfile)
	bs, err := os.ReadFile(outfile)
	t.R.NoError(err)
	output = string(bs)

	t.A.Contains(output, "stdout")
	t.A.Contains(output, "stderr")
}

func TestRunModelCommandIgnoreError(tt *testing.T) {
	t := wrapt.WrapT(tt)

	skipIfNoSh(t)

	cmd := exec.Command("sh", "-c", "printf 'stdout\n'; printf >&2 'IGNORE\n'; exit 1")
	mod := &NonMemModel{Model: "foo.ctl", FileName: "foo", OutputDir: t.TempDir()}

	ign := func(_ error, output string) bool {
		return strings.Contains(output, "IGNORE")
	}

	cerr := runModelCommand(mod, cmd, ign)
	t.A.Equal(cerr, turnstile.ConcurrentError{})

	outfile := filepath.Join(mod.OutputDir, mod.Model+".out")
	t.R.FileExists(outfile)
	bs, err := os.ReadFile(outfile)
	t.R.NoError(err)
	output = string(bs)

	t.A.Contains(output, "stdout")
	t.A.Contains(output, "IGNORE")
}
