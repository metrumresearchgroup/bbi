package parser

import (
	"fmt"
	"testing"
)

func TestAddingPathLevel(t *testing.T) {
	originalPaths := []string{
		"$DATA modeling/data1.csv",
		"$DATA /usr/modeling/data1.csv",
		"$DATA ../modeling/data1.csv",
	}
	newPaths := []string{
		"$DATA ../modeling/data1.csv",
		"$DATA /usr/modeling/data1.csv",
		"$DATA ../../modeling/data1.csv",
	}
	for i, val := range originalPaths {
		fmt.Println(i, AddPathLevelToData(val), "   should be:   ", newPaths[i])
	}
}
