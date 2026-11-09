package nmless

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"testing"

	"github.com/metrumresearchgroup/bbi/configlib"
	bi "github.com/metrumresearchgroup/bbi/integration"
	"github.com/metrumresearchgroup/wrapt"
)

func dirnames(t *testing.T, dir string) []string {
	t.Helper()

	fh, err := os.Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	defer fh.Close()

	files, err := fh.Readdirnames(0)
	if err != nil {
		t.Fatal(err)
	}

	slices.Sort(files)

	return files
}

func TestRecleanBasic(tt *testing.T) {
	t := wrapt.WrapT(tt)

	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "FDATA"), []byte{}, 0o666)
	t.R.NoError(err)
	err = os.WriteFile(filepath.Join(dir, "FDATA.csv"), []byte{}, 0o666)
	t.R.NoError(err)

	err = configlib.WriteViperConfig(dir, false, configlib.Config{})
	t.R.NoError(err)

	t.A.Equal([]string{"FDATA", "FDATA.csv", "bbi.yaml"}, dirnames(tt, dir))

	output, err := bi.ExecuteCommand(context.Background(),
		"bbi", "nonmem", "reclean", "--lvl=0", "-v", dir)
	t.R.NoError(err)
	t.A.NotEmpty(output)

	t.A.Equal([]string{"FDATA", "FDATA.csv", "bbi.yaml"}, dirnames(tt, dir))

	output, err = bi.ExecuteCommand(context.Background(),
		"bbi", "nonmem", "reclean", "--lvl=0", "--preview", dir)
	t.R.NoError(err)
	t.A.NotEmpty(output)

	t.A.Equal([]string{"FDATA", "FDATA.csv", "bbi.yaml"}, dirnames(tt, dir))

	output, err = bi.ExecuteCommand(context.Background(),
		"bbi", "nonmem", "reclean", "--lvl=1", "-v", dir)
	t.R.NoError(err)
	t.A.NotEmpty(output)

	t.A.Equal([]string{"bbi.yaml"}, dirnames(tt, dir))
}

func TestRecleanFromConfig(tt *testing.T) {
	t := wrapt.WrapT(tt)

	dir := t.TempDir()

	err := os.WriteFile(filepath.Join(dir, "WK_22"), []byte{}, 0o666)
	t.R.NoError(err)
	err = os.WriteFile(filepath.Join(dir, "licfile.set"), []byte{}, 0o666)
	t.R.NoError(err)

	cfg := configlib.Config{CleanLvl: 1, Parallel: true}
	err = configlib.WriteViperConfig(dir, false, cfg)
	t.R.NoError(err)

	t.A.Equal([]string{"WK_22", "bbi.yaml", "licfile.set"}, dirnames(tt, dir))

	_, err = bi.ExecuteCommand(context.Background(),
		"bbi", "nonmem", "reclean", dir)
	t.R.NoError(err)

	t.A.Equal([]string{"bbi.yaml"}, dirnames(tt, dir))
}

func TestRecleanError(tt *testing.T) {
	t := wrapt.WrapT(tt)
	tdir := t.TempDir()
	fname := filepath.Join(tdir, "afile")
	err := os.WriteFile(fname, []byte{}, 0o666)
	t.R.NoError(err)

	tests := []struct {
		args []string
		want string
	}{
		{
			args: []string{},
			want: "one positional",
		},
		{
			args: []string{filepath.Join(tdir, "idonotexist")},
			want: "idonotexist",
		},
		{
			args: []string{fname},
			want: "not a directory",
		},
	}

	for i, tc := range tests {
		tt.Run(strconv.Itoa(i), func(tt *testing.T) {
			t := wrapt.WrapT(tt)
			args := append([]string{"nonmem", "reclean"}, tc.args...)
			output, err := bi.ExecuteCommandNoErrorCheck(
				context.Background(),
				"bbi", args...)
			t.R.NotNil(err)
			t.A.Contains(output, tc.want)
		})
	}
}
