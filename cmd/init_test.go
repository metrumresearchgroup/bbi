package cmd

import (
	"errors"
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/metrumresearchgroup/bbi/configlib"
	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/wrapt"
)

func TestFindNonMemBinaryWindows(tt *testing.T) {
	t := wrapt.WrapT(tt)
	dir := t.TempDir()
	rdir := filepath.Join(dir, "run")
	if err := os.Mkdir(rdir, 0777); err != nil {
		t.Fatal(err)
	}

	foo := filepath.Join(rdir, "foo.bat")
	if err := os.WriteFile(foo, []byte(""), 0666); err != nil {
		t.Fatal(err)
	}

	_, err := findNonMemBinaryWindows(dir)
	t.R.Error(err)
	t.R.Contains(err.Error(), ".bat file not found")

	nmfe := filepath.Join(rdir, "nmfe23.bat")
	if err = os.WriteFile(nmfe, []byte(""), 0666); err != nil {
		t.Fatal(err)
	}

	binary, err := findNonMemBinaryWindows(dir)
	t.R.NoError(err)
	t.R.Equal(binary, "nmfe23.bat")
}

func setupDir(dir string) error {
	for _, d := range []string{"license", "run", "util", "source"} {
		sdir := filepath.Join(dir, d)
		if err := os.MkdirAll(sdir, 0777); err != nil {
			return err
		}
	}

	lic := filepath.Join(dir, "license", "nonmem.lic")
	if err := os.WriteFile(lic, []byte(""), 0666); err != nil {
		return err
	}

	nmfe := filepath.Join(dir, "run", "nmfe12")
	if runtime.GOOS == "windows" {
		nmfe = nmfe + ".bat"
	}

	if err := os.WriteFile(nmfe, []byte(""), 0777); err != nil {
		return err
	}

	return nil
}

func TestInitMakeNonmemEntries(tt *testing.T) {
	id := "UNIT-INIT-001"
	tt.Run(utils.AddTestId("", id), func(tt *testing.T) {
		t := wrapt.WrapT(tt)
		tdir := t.TempDir()

		name := "nm75"
		if err := setupDir(filepath.Join(tdir, name)); err != nil {
			t.Fatal(err)
		}

		es, err := makeNonmemEntries([]string{tdir})
		t.R.NoError(err)
		t.R.Equal(es[name].Home, filepath.Join(tdir, name))
	})
}

func TestInitMakeNonmemEntriesMultiple(tt *testing.T) {
	id := "UNIT-INIT-002"
	tt.Run(utils.AddTestId("", id), func(tt *testing.T) {
		t := wrapt.WrapT(tt)
		tdir := t.TempDir()

		d1 := filepath.Join(tdir, "d1")
		if err := os.Mkdir(d1, 0777); err != nil {
			t.Fatal(err)
		}

		d2 := filepath.Join(tdir, "d2")
		if err := os.Mkdir(d2, 0777); err != nil {
			t.Fatal(err)
		}

		dirs := []string{d1, d2}
		names := []string{"nm1", "nm2"}
		for i, d := range dirs {
			if err := setupDir(filepath.Join(d, names[i])); err != nil {
				t.Fatal(err)
			}
		}

		es, err := makeNonmemEntries(dirs)
		t.R.NoError(err)
		t.R.Equal(es[names[0]].Home, filepath.Join(d1, names[0]))
		t.R.Equal(es[names[1]].Home, filepath.Join(d2, names[1]))
	})
}

func keys(m map[string]configlib.NonMemDetail) []string {
	ks := make([]string, len(m))
	i := 0
	for k := range m {
		ks[i] = k
		i++
	}

	return ks
}

func TestInitMakeNonmemEntriesCollisionAcrossDirs(tt *testing.T) {
	id := "UNIT-INIT-003"
	tt.Run(utils.AddTestId("", id), func(tt *testing.T) {
		t := wrapt.WrapT(tt)
		tdir := t.TempDir()

		d1 := filepath.Join(tdir, "d1")
		if err := os.Mkdir(d1, 0777); err != nil {
			t.Fatal(err)
		}

		d2 := filepath.Join(tdir, "d2")
		if err := os.Mkdir(d2, 0777); err != nil {
			t.Fatal(err)
		}

		name := "nm75"
		dirs := []string{d1, d2}
		for _, d := range dirs {
			if err := setupDir(filepath.Join(d, name)); err != nil {
				t.Fatal(err)
			}
		}

		es, err := makeNonmemEntries(dirs)
		t.R.NoError(err)
		t.R.Equal(keys(es), []string{name})
	})
}

func TestInitMakeNonmemEntriesCollisionCase(tt *testing.T) {
	id := "UNIT-INIT-004"
	tt.Run(utils.AddTestId("", id), func(tt *testing.T) {
		t := wrapt.WrapT(tt)
		tdir := t.TempDir()

		if err := setupDir(filepath.Join(tdir, "NM75")); err != nil {
			t.Fatal(err)
		}

		if _, err := os.Stat(filepath.Join(tdir, "nm75")); err == nil {
			t.Skip("Cannot test on case-insensitive file system")
		} else if !errors.Is(err, os.ErrNotExist) {
			t.Fatal(err)
		}

		if err := setupDir(filepath.Join(tdir, "nm75")); err != nil {
			t.Fatal(err)
		}

		es, err := makeNonmemEntries([]string{tdir})
		t.R.NoError(err)
		t.R.Equal(keys(es), []string{"nm75"})
	})
}
