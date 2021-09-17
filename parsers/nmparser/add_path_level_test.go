package parser

import (
	"runtime"
	"strconv"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestAddingPathLevel(tt *testing.T) {
	originalPaths := []string{
		"$DATA modeling/data1.csv",
		"$DATA /usr/modeling/data1.csv",
		"$DATA ../modeling/data1.csv",
		"$DATA modeling/data1.csv IGNORE=@",
		"$DATA modeling/data1.csv IGNORE=@ IGNORE=ID.GT.5",
	}
	var newPaths []string
	if runtime.GOOS == "windows" {
		newPaths = []string{
			"$DATA ..\\modeling\\data1.csv",
			"$DATA /usr/modeling/data1.csv",
			"$DATA ..\\..\\modeling\\data1.csv",
			"$DATA ..\\modeling\\data1.csv IGNORE=@",
			"$DATA ..\\modeling\\data1.csv IGNORE=@ IGNORE=ID.GT.5",
		}
	} else {
		newPaths = []string{
			"$DATA ../modeling/data1.csv",
			"$DATA /usr/modeling/data1.csv",
			"$DATA ../../modeling/data1.csv",
			"$DATA ../modeling/data1.csv IGNORE=@",
			"$DATA ../modeling/data1.csv IGNORE=@ IGNORE=ID.GT.5",
		}
	}
	for i, val := range originalPaths {
		tt.Run("path "+strconv.Itoa(i), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			newPath := AddPathLevelToData(val)

			t.R.Equal(newPaths[i], newPath)
		})
	}
}
