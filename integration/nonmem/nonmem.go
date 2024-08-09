package nonmem

import (
	"crypto/md5"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/metrumresearchgroup/bbi/runner"

	"github.com/metrumresearchgroup/wrapt"
	"github.com/spf13/afero"
)

type NonMemTestingDetails struct {
	OutputDir string
	Model     Model
	Output    string
	Scenario  *Scenario
}

func AssertNonMemCompleted(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	nmlines, err := fileLines(filepath.Join(details.OutputDir, details.Model.identifier+".lst"))

	t.R.NoError(err)
	t.R.NotNil(nmlines)
	t.R.NotEmpty(nmlines)

	// Check for either finaloutput or Stop Time as Stop Time appears in older versions of nonmem

	t.R.True(strings.Contains(strings.Join(nmlines, "\n"), "finaloutput") || strings.Contains(strings.Join(nmlines, "\n"), "Stop Time:"))
}

func AssertNonMemCreatedOutputFiles(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	fs := afero.NewOsFs()
	expected := []string{
		".xml",
		".cpu",
		".grd",
	}

	for _, v := range expected {
		ok, _ := afero.Exists(fs, filepath.Join(details.OutputDir, details.Model.identifier+v))
		t.R.True(ok, "Unable to locate expected file %s", v)
	}
}

func AssertNonMemCleanedUpFiles(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	for f, level := range runner.EstOutputFileCleanLevels(details.Model.identifier) {
		if level == 1 {
			t.A.NoFileExists(filepath.Join(details.OutputDir, f))
		}
	}

	// Parallelization-related files handled by nonmem.filesToCleanup().
	files, err := os.ReadDir(details.OutputDir)
	t.R.NoError(err)
	for _, fi := range files {
		t.R.NotRegexp("^WK_", fi.Name())
	}
}

func AssertBBIConfigJSONCreated(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	fs := afero.NewOsFs()

	ok, _ := afero.Exists(fs, filepath.Join(details.OutputDir, "bbi_config.json"))
	t.R.True(ok)
}

func AssertContainsBBIScript(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	fs := afero.NewOsFs()

	ok, _ := afero.Exists(fs, filepath.Join(details.OutputDir, details.Model.identifier+".sh"))
	t.R.True(ok, "The required BBI execution script %s, is not present in the output dir", details.Model.identifier+".sh")
}

func AssertNonMemOutputContainsParafile(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	containsParafile := false

	lines, _ := fileLines(filepath.Join(details.OutputDir, details.Model.identifier+".lst"))

	for _, v := range lines {
		if strings.Contains(v, "PARAFILE=") {
			containsParafile = true
		}
	}

	t.R.True(containsParafile)
}

func AssertDefaultConfigLoaded(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	t.R.Contains(details.Output, "Successfully loaded default configuration")
}

func AssertSpecifiedConfigLoaded(t *wrapt.T, details NonMemTestingDetails, specificFile string) {
	t.Helper()

	message := fmt.Sprintf("Successfully loaded specified configuration from %s", specificFile)
	t.R.Contains(details.Output, message)
}

func AssertContainsNMFEOptions(t *wrapt.T, filepath string, optionValue string) {
	t.Helper()

	content, _ := os.ReadFile(filepath)
	contentString := string(content)
	t.R.Contains(contentString, optionValue)
}

// Make sure that the BBIConfig json has a value for the data hash.
func AssertDataSourceIsHashedAndCorrect(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	file, _ := os.Open(filepath.Join(details.Scenario.Workpath, "scenario.json"))
	originalDetails, err := GetScenarioDetailsFromFile(file)
	t.R.NoError(err)

	bbiConfigJson, err := os.Open(filepath.Join(details.OutputDir, "bbi_config.json"))
	t.R.NoError(err)

	savedHashes, err := GetBBIConfigJSONHashedValues(bbiConfigJson)
	t.R.NoError(err)

	t.R.NotEmpty(savedHashes.Data)

	// Get MD5 of current file
	datafile, _ := os.Open(filepath.Join(details.Scenario.Workpath, originalDetails.DataFile))
	defer datafile.Close()
	dataHash, _ := calculateMD5(datafile)

	// Make sure the calculated and saved values are the same
	t.R.Equal(savedHashes.Data, dataHash)
}

func AssertModelIsHashedAndCorrect(t *wrapt.T, details NonMemTestingDetails) {
	t.Helper()

	bbiConfigJson, _ := os.Open(filepath.Join(details.OutputDir, "bbi_config.json"))
	savedHashes, err := GetBBIConfigJSONHashedValues(bbiConfigJson)
	t.R.NoError(err)

	t.R.NotEmpty(savedHashes.Model)

	// Get MD5 of model ORIGINAL file
	// Getting a hash of the model's relative copy is pointless since we may or may not have modified it's $DATA location
	model, _ := os.Open(filepath.Join(details.Scenario.Workpath, details.Model.filename))
	defer model.Close()

	calculatedHash, _ := calculateMD5(model)

	t.R.Equal(savedHashes.Model, calculatedHash)
}

func calculateMD5(r io.Reader) (string, error) {
	h := md5.New()

	if _, err := io.Copy(h, r); err != nil {
		return "", err
	}

	return fmt.Sprintf("%x", h.Sum(nil)), nil
}
