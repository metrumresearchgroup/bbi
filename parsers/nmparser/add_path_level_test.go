package parser

import (
	"github.com/metrumresearchgroup/bbi/utils"
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

	testId := "UNIT-NMP-013"
	for i, val := range originalPaths {
		tt.Run(utils.AddTestId("path "+strconv.Itoa(i), testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			newPath := AddPathLevelToData(val)

			t.R.Equal(newPaths[i], newPath)
		})
	}
}
