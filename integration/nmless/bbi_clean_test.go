package nmless

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/wrapt"

	"github.com/metrumresearchgroup/bbi/runner"
)

func createEmptyFile(t *wrapt.T, file string) {
	t.Helper()
	fh, err := os.Create(file)
	if err != nil {
		t.Fatal(err)
	}
	err = fh.Close()
	if err != nil {
		t.Fatal(err)
	}
}

func TestClean(tt *testing.T) {
	t := wrapt.WrapT(tt)

	t.Run("glob", func(t *wrapt.T) {
		dir := t.TempDir()

		foo := filepath.Join(dir, "foo")
		fooctl := filepath.Join(dir, "foo.ctl")
		foomod := filepath.Join(dir, "foo.mod")

		createEmptyFile(t, foo)
		createEmptyFile(t, fooctl)
		createEmptyFile(t, foomod)

		cmd := exec.Command("bbi", "nonmem", "clean", "*.ctl", "*.mod")
		cmd.Dir = dir

		_, err := cmd.Output()
		if err != nil {
			t.Fatal(err)
		}

		t.A.FileExists(foo)
		t.A.NoFileExists(fooctl)
		t.A.NoFileExists(foomod)
	})

	t.Run("regex", func(t *wrapt.T) {
		dir := t.TempDir()

		fooctl := filepath.Join(dir, "foo.ctl")
		foolmod := filepath.Join(dir, "foolmod")
		foomod := filepath.Join(dir, "foo.mod")
		foomodulo := filepath.Join(dir, "foo.modulo")

		createEmptyFile(t, fooctl)
		createEmptyFile(t, foolmod)
		createEmptyFile(t, foomod)
		createEmptyFile(t, foomodulo)

		cmd := exec.Command("bbi", "nonmem", "clean", "--regex", ".*\\.ctl", ".*\\.mod$")
		cmd.Dir = dir

		_, err := cmd.Output()
		if err != nil {
			t.Fatal(err)
		}

		t.A.NoFileExists(fooctl)
		t.A.FileExists(foolmod)
		t.A.NoFileExists(foomod)
		t.A.FileExists(foomodulo)
	})

	t.Run("glob inverse", func(t *wrapt.T) {
		dir := t.TempDir()

		foo := filepath.Join(dir, "foo")
		fooctl := filepath.Join(dir, "foo.ctl")

		createEmptyFile(t, foo)
		createEmptyFile(t, fooctl)

		cmd := exec.Command("bbi", "nonmem", "clean", "--inverse", "*.ctl")
		cmd.Dir = dir

		_, err := cmd.Output()
		if err != nil {
			t.Fatal(err)
		}

		t.A.NoFileExists(foo)
		t.A.FileExists(fooctl)
	})

	t.Run("regex inverse", func(t *wrapt.T) {
		dir := t.TempDir()

		barfoo := filepath.Join(dir, "barfoo.mod")
		foomodulo := filepath.Join(dir, "foo.modulo")

		createEmptyFile(t, foomodulo)
		createEmptyFile(t, barfoo)

		cmd := exec.Command("bbi", "nonmem", "clean", "--regex", "--inverse", "^foo.*mod")
		cmd.Dir = dir

		_, err := cmd.Output()
		if err != nil {
			t.Fatal(err)
		}

		t.A.NoFileExists(barfoo)
		t.A.FileExists(foomodulo)
	})

	t.Run("dirsOnly", func(t *wrapt.T) {
		dir := t.TempDir()

		foodir := filepath.Join(dir, "foo")
		foomod := filepath.Join(dir, "foo.mod")

		err := os.Mkdir(foodir, 0o777)
		if err != nil {
			t.Fatal(err)
		}

		createEmptyFile(t, foomod)

		cmd := exec.Command("bbi", "nonmem", "clean", "--dirsOnly", "foo*")
		cmd.Dir = dir

		_, err = cmd.Output()
		if err != nil {
			t.Fatal(err)
		}

		t.A.NoDirExists(foodir)
		t.A.FileExists(foomod)
	})

	t.Run("filesOnly", func(t *wrapt.T) {
		dir := t.TempDir()

		foodir := filepath.Join(dir, "foo")
		foomod := filepath.Join(dir, "foo.mod")

		err := os.Mkdir(foodir, 0o777)
		if err != nil {
			t.Fatal(err)
		}

		createEmptyFile(t, foomod)

		cmd := exec.Command("bbi", "nonmem", "clean", "--filesOnly", "foo*")
		cmd.Dir = dir

		_, err = cmd.Output()
		if err != nil {
			t.Fatal(err)
		}

		t.A.DirExists(foodir)
		t.A.NoFileExists(foomod)
	})
}

func writeCopied(t *wrapt.T, prefix string, files []runner.TargetedFile) {
	t.Helper()

	bs, err := json.Marshal(files)
	if err != nil {
		t.Fatal(err)
	}
	err = os.WriteFile(prefix+"_copied.json", bs, 0o666)
	if err != nil {
		t.Fatal(err)
	}
}

func TestCleanCopiedRuns(tt *testing.T) {
	t := wrapt.WrapT(tt)
	dir := t.TempDir()

	run01 := filepath.Join(dir, "run01.mod")
	run01foo := filepath.Join(dir, "run01.foo")
	run02 := filepath.Join(dir, "run02.mod")
	run02bar := filepath.Join(dir, "run02.bar")
	run03 := filepath.Join(dir, "run03.mod")
	run03keep := filepath.Join(dir, "run03.keep")
	run10 := filepath.Join(dir, "run10.mod")
	run10baz := filepath.Join(dir, "run10.baz")

	createEmptyFile(t, run01)
	createEmptyFile(t, run01foo)
	createEmptyFile(t, run02)
	createEmptyFile(t, run02bar)
	createEmptyFile(t, run03)
	createEmptyFile(t, run03keep)
	createEmptyFile(t, run10)
	createEmptyFile(t, run10baz)

	writeCopied(t, filepath.Join(dir, "run01"),
		[]runner.TargetedFile{
			{
				File: "run01.foo",
			},
		},
	)
	writeCopied(t, filepath.Join(dir, "run02"),
		[]runner.TargetedFile{
			{
				File: "run02.bar",
			},
		},
	)
	writeCopied(t, filepath.Join(dir, "run03"),
		[]runner.TargetedFile{
			{
				File: "run03.keep",
			},
		},
	)
	writeCopied(t, filepath.Join(dir, "run10"),
		[]runner.TargetedFile{
			{
				File: "run10.baz",
			},
		},
	)

	cmd := exec.Command("bbi", "nonmem", "clean", "--copiedRuns=run[01:02],run10")
	cmd.Dir = dir

	_, err := cmd.Output()
	if err != nil {
		t.Fatal(err)
	}

	t.A.FileExists(run01)
	t.A.NoFileExists(run01foo)

	t.A.FileExists(run02)
	t.A.NoFileExists(run02bar)

	t.A.FileExists(run03)
	t.A.FileExists(run03keep)

	t.A.FileExists(run10)
	t.A.NoFileExists(run10baz)
}

func TestCleanPreview(tt *testing.T) {
	t := wrapt.WrapT(tt)
	dir := t.TempDir()

	fooctl := filepath.Join(dir, "foo.ctl")
	foomod := filepath.Join(dir, "foo.mod")
	other := filepath.Join(dir, "other")

	createEmptyFile(t, fooctl)
	createEmptyFile(t, foomod)
	createEmptyFile(t, other)

	cmd := exec.Command("bbi", "nonmem", "clean", "--preview", "*.ctl", "*.mod")
	cmd.Dir = dir

	bs, err := cmd.Output()
	if err != nil {
		t.Fatal(err)
	}
	out := string(bs)

	t.A.Contains(out, "foo.ctl")
	t.A.Contains(out, "foo.mod")
	t.A.NotContains(out, "other")

	t.A.FileExists(fooctl)
	t.A.FileExists(foomod)
	t.A.FileExists(other)
}
