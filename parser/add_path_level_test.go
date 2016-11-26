package parser

import "testing"

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
		newPath := AddPathLevelToData(val)
		if newPath != newPaths[i] {
			t.Log("GOT: ", newPath, " EXPECTED: ", newPaths[i])
			t.Fail()
		}
	}
}
