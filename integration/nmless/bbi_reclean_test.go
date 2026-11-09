package nmless

import (
	"context"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"testing"

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

	t.A.Equal([]string{"FDATA", "FDATA.csv"}, dirnames(tt, dir))

	output, err := bi.ExecuteCommand(context.Background(),
		"bbi", "nonmem", "reclean", "--lvl=1", "-v", dir)
	t.R.NoError(err)
	t.A.NotEmpty(output)

	t.A.Equal([]string{}, dirnames(tt, dir))
}

func TestRecleanError(tt *testing.T) {
	t := wrapt.WrapT(tt)

	output, err := bi.ExecuteCommandNoErrorCheck(context.Background(),
		"bbi", "nonmem", "reclean")

	t.R.NotNil(err)
	errorMatch, _ := regexp.MatchString("one positional", output)
	t.A.True(errorMatch)
}
