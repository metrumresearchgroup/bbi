package bbitest

import (
	"io/ioutil"
	"os"
	"strings"
)

func fileLines(path string) ([]string, error) {
	file, err := os.Open(path)

	if err != nil {
		return []string{}, err
	}

	defer file.Close()

	contentBytes, err := ioutil.ReadAll(file)

	if err != nil {
		return []string{}, err
	}

	contentLines := strings.Split(string(contentBytes), "\n")

	return contentLines, nil
}
