package utils

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strings"

	"github.com/spf13/afero"
)

//ReadParamsAndOutputFromExt returns the lines associated
// with either parameter table outputs or output lines (-100xxx lines).
//
func ReadParamsAndOutputFromExt(path string) ([]string, error) {
	inFile, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer inFile.Close()
	scanner := bufio.NewScanner(inFile)

	// increase the max buffer size to accommodate very wide tables (lots of params)
	buf := make([]byte, 0, 16*1024)
	scanner.Buffer(buf, 256*1024)

	scanner.Split(bufio.ScanLines)
	var lines []string
	for scanner.Scan() {
		txt := scanner.Text()
		switch {
		case strings.HasPrefix(txt, "TABLE"):
			lines = append(lines, scanner.Text())
		case strings.HasPrefix(txt, " ITER"):
			lines = append(lines, scanner.Text())
		case strings.HasPrefix(txt, "  -100000000"):
			lines = append(lines, scanner.Text())
		default:
			continue
		}
	}

	// explicitly handle line that's too long
	if errors.Is(scanner.Err(), bufio.ErrTooLong) {
		err = fmt.Errorf("Attempting to parse %s with ReadParamsAndOutputFromExt(): %w", path, scanner.Err())

		return nil, err
	}

	// if file was empty prompt about renamed .ext file
	if lines == nil {
		emptyFileMsg := fmt.Sprintf("A file exists at %s but it is empty.\n", path)
		renameExtMsg := "If you sent NONMEM output to a different file you can use --ext-file=NEWFILE to specify the new file name.\n"
		err = errors.New(emptyFileMsg + renameExtMsg)

		return nil, err
	}

	return lines, nil
}

//ReadLines reads lines for a file at a given path.
func ReadLines(path string) ([]string, error) {
	inFile, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer inFile.Close()
	scanner := bufio.NewScanner(inFile)
	scanner.Split(bufio.ScanLines)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	return lines, nil
}

//ReadLinesFS reads lines for a file at a given path.
func ReadLinesFS(fs afero.Fs, path string) ([]string, error) {
	inFile, err := fs.Open(path)
	if err != nil {
		return nil, err
	}
	defer inFile.Close()
	scanner := bufio.NewScanner(inFile)
	scanner.Split(bufio.ScanLines)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	return lines, nil
}

//WriteLines writes lines to a file at a given path given a filesystem.
func WriteLines(lines []string, path string) error {
	file, err := os.Create(path)
	if err != nil {
		return err
	}
	defer file.Close()

	w := bufio.NewWriter(file)
	for _, line := range lines {
		fmt.Fprintln(w, line)
	}

	return w.Flush()
}

//WriteLinesFS writes lines to a file at a given path given a filesystem.
func WriteLinesFS(fs afero.Fs, lines []string, path string) error {
	file, err := fs.Create(path)
	if err != nil {
		return err
	}
	defer file.Close()

	w := bufio.NewWriter(file)
	for _, line := range lines {
		fmt.Fprintln(w, line)
	}

	return w.Flush()
}

// HasZero returns true if any float in the slice is zero.
func HasZero(floats []float64) bool {
	for _, f := range floats {
		if f == 0 {
			return true
		}
	}

	return false
}
