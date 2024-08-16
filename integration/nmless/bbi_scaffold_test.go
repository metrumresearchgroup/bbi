package nmless

import (
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestScaffoldPreview(tt *testing.T) {
	t := wrapt.WrapT(tt)
	dir := t.TempDir()

	cmd := exec.Command("bbi", "nonmem", "scaffold", "--cacheDir=foo", "--preview")
	cmd.Dir = dir

	bs, err := cmd.Output()
	if err != nil {
		t.Fatal(err)
	}
	out := string(bs)

	t.A.Contains(out, "would create")
	t.A.Contains(out, "foo")

	t.A.NoDirExists(filepath.Join(dir, "foo"))
}

func TestScaffold(tt *testing.T) {
	t := wrapt.WrapT(tt)
	dir := t.TempDir()

	cmd := exec.Command("bbi", "nonmem", "scaffold", "--cacheDir=foo/bar")
	cmd.Dir = dir

	_, err := cmd.Output()
	if err != nil {
		t.Fatal(err)
	}

	t.A.DirExists(filepath.Join(dir, "foo", "bar"))
	t.A.FileExists(filepath.Join(dir, "foo", "bar", ".gitignore"))
}
