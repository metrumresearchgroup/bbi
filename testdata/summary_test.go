package cmd

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"path/filepath"
	"testing"

	parser "github.com/metrumresearchgroup/babylon/parsers/nmparser"
	"github.com/spf13/afero"
	"github.com/stretchr/testify/assert"
)

func TestSummary(t *testing.T) {
	var tests = []struct {
		modFile    string
		goldenFile string
	}{
		{
			modFile:    "./example-models/nonmem/74/meropenem/meropenem.mod",
			goldenFile: "./example-models/nonmem/74/meropenem/meropenem.json",
		},
		{
			modFile:    "./example-models/nonmem/74/BQL/2.mod",
			goldenFile: "./example-models/nonmem/74/BQL/BQL.json",
		},
		// does not generate shk
		// {
		// 	modFile:    "./example-models/nonmem/74/IOVMM/10.mod",
		// 	goldenFile: "./example-models/nonmem/74/IOVMM/IOVMM.json",
		// },
		{
			modFile:    "./example-models/nonmem/74/TMDD/1.mod",
			goldenFile: "./example-models/nonmem/74/TMDD/TMDD.json",
		},
		{
			modFile:    "./example-models/nonmem/74/NonLinearCL/1.mod",
			goldenFile: "./example-models/nonmem/74/NonLinearCL/NonLinearCL.json",
		},
	}
	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, tt := range tests {
		context := filepath.Base(tt.goldenFile)

		// read bytes from golden json
		goldenJSON, err := afero.ReadFile(osFs, tt.goldenFile)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] file error %s: %s", context, tt.goldenFile, err))

		// unmarshal golden json
		var goldenSummary parser.ModelOutput
		err = json.Unmarshal(goldenJSON, &goldenSummary)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to unmarshal goldenSummary: %s", context, err))

		// execute bbi and capture summary output as string
		stdout, err := exec.Command(bbiExe, "summary", "--json", tt.modFile).Output()
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail exec %s: %s", context, bbiExe, err))

		// unmarshal summary output
		var bbiSummary parser.ModelOutput
		err = json.Unmarshal(stdout, &bbiSummary)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to unmarshal summary: %s", context, err))

		//compare objects
		assert.Equal(t, goldenSummary.ParametersData, bbiSummary.ParametersData, fmt.Sprintf("[%s] parametersData not equal", context))
		assert.Equal(t, goldenSummary.ParameterStructures, bbiSummary.ParameterStructures, fmt.Sprintf("[%s] parametersStructures not equal", context))
		assert.Equal(t, goldenSummary.ParameterNames, bbiSummary.ParameterNames, fmt.Sprintf("[%s] parametersStructures not equal", context))
		assert.Equal(t, goldenSummary.OFV, bbiSummary.OFV, fmt.Sprintf("[%s] parametersStructures not equal", context))
		assert.Equal(t, goldenSummary.ShrinkageDetails, bbiSummary.ShrinkageDetails, fmt.Sprintf("[%s] parametersStructures not equal", context))

		//compare file
		b := assert.ObjectsAreEqual(goldenJSON, stdout)
		assert.Equal(t, true, b, fmt.Sprintf("[%s] JSON goldenfile not equal to JSON output", tt.goldenFile))
		if b == false {
			fmt.Printf("%s", string(stdout))
		}
	}
}

func TestSummary73(t *testing.T) {
	var tests = []struct {
		modFile    string
		goldenFile string
	}{
		{
			modFile:    "./example-models/nonmem/73/meropenem/meropenem.mod",
			goldenFile: "./example-models/nonmem/73/meropenem/meropenem.json",
		},
		{
			modFile:    "./example-models/nonmem/73/BQL/2.mod",
			goldenFile: "./example-models/nonmem/73/BQL/BQL.json",
		},
		// no shk file
		// {
		// 	modFile:    "./example-models/nonmem/73/IOVMM/10.mod",
		// 	goldenFile: "./example-models/nonmem/73/IOVMM/IOVMM.json",
		// },
		// {
		// 	modFile:    "./example-models/nonmem/73/IOVMM_COV/10.mod",
		// 	goldenFile: "./example-models/nonmem/73/IOVMM_COV/IOVMM_COV.json",
		// },
		{
			modFile:    "./example-models/nonmem/73/TMDD/1.mod",
			goldenFile: "./example-models/nonmem/73/TMDD/TMDD.json",
		},
		{
			modFile:    "./example-models/nonmem/73/NonLinearCL/1.mod",
			goldenFile: "./example-models/nonmem/73/NonLinearCL/NonLinearCL.json",
		},
	}
	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, tt := range tests {
		context := filepath.Base(tt.goldenFile)

		// read bytes from golden json
		goldenJSON, err := afero.ReadFile(osFs, tt.goldenFile)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] file error %s: %s", context, tt.goldenFile, err))

		// unmarshal golden json
		var goldenSummary parser.ModelOutput
		err = json.Unmarshal(goldenJSON, &goldenSummary)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to unmarshal goldenSummary: %s", context, err))

		// execute bbi and capture summary output as string
		stdout, err := exec.Command(bbiExe, "summary", "--json", "--no-grd-file", tt.modFile).Output()
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail exec %s: %s", context, bbiExe, err))

		// unmarshal summary output
		var bbiSummary parser.ModelOutput
		err = json.Unmarshal(stdout, &bbiSummary)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to unmarshal summary: %s", context, err))

		//compare objects
		assert.Equal(t, goldenSummary.ParametersData, bbiSummary.ParametersData, fmt.Sprintf("[%s] parametersData not equal", context))
		assert.Equal(t, goldenSummary.ParameterStructures, bbiSummary.ParameterStructures, fmt.Sprintf("[%s] parametersStructures not equal", context))
		assert.Equal(t, goldenSummary.ParameterNames, bbiSummary.ParameterNames, fmt.Sprintf("[%s] parametersStructures not equal", context))
		assert.Equal(t, goldenSummary.OFV, bbiSummary.OFV, fmt.Sprintf("[%s] parametersStructures not equal", context))
		assert.Equal(t, goldenSummary.ShrinkageDetails, bbiSummary.ShrinkageDetails, fmt.Sprintf("[%s] parametersStructures not equal", context))

		//compare file
		b := assert.ObjectsAreEqual(goldenJSON, stdout)
		assert.Equal(t, true, b, fmt.Sprintf("[%s] JSON goldenfile not equal to JSON output", tt.goldenFile))
		if b == false {
			fmt.Printf("%s", string(stdout))
		}
	}
}

func TestSummaryTable(t *testing.T) {
	var tests = []struct {
		modFile    string
		goldenFile string
	}{
		{
			modFile:    "./example-models/nonmem/74/meropenem/meropenem.mod",
			goldenFile: "./example-models/nonmem/74/meropenem/meropenem_table.txt",
		},
		{
			modFile:    "./example-models/nonmem/74/BQL/2.mod",
			goldenFile: "./example-models/nonmem/74/BQL/BQL_table.txt",
		},
		// {
		// 	modFile:    "./example-models/nonmem/74/IOVMM/10.mod",
		// 	goldenFile: "./example-models/nonmem/74/IOVMM/IOVMM_table.txt",
		// },
		{
			modFile:    "./example-models/nonmem/74/TMDD/1.mod",
			goldenFile: "./example-models/nonmem/74/TMDD/TMDD_table.txt",
		},
		{
			modFile:    "./example-models/nonmem/74/NonLinearCL/1.mod",
			goldenFile: "./example-models/nonmem/74/NonLinearCL/NonLinearCL_table.txt",
		},
	}
	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, tt := range tests {
		context := filepath.Base(tt.goldenFile)

		// read bytes from golden txt
		goldenJSON, err := afero.ReadFile(osFs, tt.goldenFile)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] file error %s: %s", context, tt.goldenFile, err))

		// execute bbi and capture summary output as string
		stdout, err := exec.Command(bbiExe, "summary", tt.modFile).Output()
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail exec %s: %s", context, bbiExe, err))

		//compare file
		b := assert.ObjectsAreEqual(goldenJSON, stdout)
		assert.Equal(t, true, b, fmt.Sprintf("[%s] txt  goldenfile not equal to table output", tt.goldenFile))
		if b == false {
			fmt.Printf("%s", string(stdout))
		}
	}
}

func TestSummaryTable73(t *testing.T) {
	var tests = []struct {
		modFile    string
		goldenFile string
	}{
		{
			modFile:    "./example-models/nonmem/73/meropenem/meropenem.mod",
			goldenFile: "./example-models/nonmem/73/meropenem/meropenem_table.txt",
		},
		{
			modFile:    "./example-models/nonmem/73/BQL/2.mod",
			goldenFile: "./example-models/nonmem/73/BQL/BQL_table.txt",
		},
		// {
		// 	modFile:    "./example-models/nonmem/73/IOVMM/10.mod",
		// 	goldenFile: "./example-models/nonmem/73/IOVMM/IOVMM_table.txt",
		// },
		// {
		// 	modFile:    "./example-models/nonmem/73/IOVMM_COV/10.mod",
		// 	goldenFile: "./example-models/nonmem/73/IOVMM_COV/IOVMM_COV_table.txt",
		// },
		{
			modFile:    "./example-models/nonmem/73/TMDD/1.mod",
			goldenFile: "./example-models/nonmem/73/TMDD/TMDD_table.txt",
		},
		{
			modFile:    "./example-models/nonmem/73/NonLinearCL/1.mod",
			goldenFile: "./example-models/nonmem/73/NonLinearCL/NonLinearCL_table.txt",
		},
	}
	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, tt := range tests {
		context := filepath.Base(tt.goldenFile)

		// read bytes from golden txt
		goldenJSON, err := afero.ReadFile(osFs, tt.goldenFile)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] file error %s: %s", context, tt.goldenFile, err))

		// execute bbi and capture summary output as string
		stdout, err := exec.Command(bbiExe, "summary", tt.modFile, "--no-grd-file").Output()
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail exec %s: %s", context, bbiExe, err))

		//compare file
		b := assert.ObjectsAreEqual(goldenJSON, stdout)
		assert.Equal(t, true, b, fmt.Sprintf("[%s] txt  goldenfile not equal to table output", tt.goldenFile))
		if b == false {
			fmt.Printf("%s", string(stdout))
		}
	}
}

func TestSummary73NoShk(t *testing.T) {
	var tests = []struct {
		modFile    string
		goldenFile string
	}{
		{
			modFile:    "./example-models/nonmem/73/IOVMM/10.mod",
			goldenFile: "./example-models/nonmem/73/IOVMM/IOVMM.json",
		},
		{
			modFile:    "./example-models/nonmem/73/IOVMM_COV/10.mod",
			goldenFile: "./example-models/nonmem/73/IOVMM_COV/IOVMM_COV.json",
		},
	}
	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, tt := range tests {
		context := filepath.Base(tt.goldenFile)

		// read bytes from golden txt
		goldenJSON, err := afero.ReadFile(osFs, tt.goldenFile)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] file error %s: %s", context, tt.goldenFile, err))

		// execute bbi and capture summary output as string
		stdout, err := exec.Command(bbiExe, "summary", tt.modFile, "--json", "--no-grd-file", "--no-shk-file").Output()
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail exec %s: %s", context, bbiExe, err))

		//compare file
		b := assert.ObjectsAreEqual(goldenJSON, stdout)
		assert.Equal(t, true, b, fmt.Sprintf("[%s] txt goldenfile not equal to table output", tt.goldenFile))
		if b == false {
			fmt.Printf("%s", string(stdout))
		}
	}
}

func TestSummary74NoShk(t *testing.T) {
	var tests = []struct {
		modFile    string
		goldenFile string
	}{
		{
			modFile:    "./example-models/nonmem/74/IOVMM/10.mod",
			goldenFile: "./example-models/nonmem/74/IOVMM/IOVMM.json",
		},
	}
	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, tt := range tests {
		context := filepath.Base(tt.goldenFile)

		// read bytes from golden txt
		goldenJSON, err := afero.ReadFile(osFs, tt.goldenFile)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] file error %s: %s", context, tt.goldenFile, err))

		// execute bbi and capture summary output as string
		stdout, err := exec.Command(bbiExe, "summary", tt.modFile, "--json", "--no-grd-file", "--no-shk-file").Output()
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail exec %s: %s", context, bbiExe, err))

		//compare file
		b := assert.ObjectsAreEqual(goldenJSON, stdout)
		assert.Equal(t, true, b, fmt.Sprintf("[%s] txt goldenfile not equal to table output", tt.goldenFile))
		if b == false {
			fmt.Printf("%s", string(stdout))
		}
	}
}

func TestSummaryTable74NoShk(t *testing.T) {
	var tests = []struct {
		modFile    string
		goldenFile string
	}{
		{
			modFile:    "./example-models/nonmem/74/IOVMM/10.mod",
			goldenFile: "./example-models/nonmem/74/IOVMM/IOVMM_table.txt",
		},
	}
	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, tt := range tests {
		context := filepath.Base(tt.goldenFile)

		// read bytes from golden txt
		goldenJSON, err := afero.ReadFile(osFs, tt.goldenFile)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] file error %s: %s", context, tt.goldenFile, err))

		// execute bbi and capture summary output as string
		stdout, err := exec.Command(bbiExe, "summary", tt.modFile, "--no-grd-file", "--no-shk-file").Output()
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail exec %s: %s", context, bbiExe, err))

		//compare file
		b := assert.ObjectsAreEqual(goldenJSON, stdout)
		assert.Equal(t, true, b, fmt.Sprintf("[%s] txt goldenfile not equal to table output", tt.goldenFile))
		if b == false {
			fmt.Printf("%s", string(stdout))
		}
	}
}

func TestSummaryTable73NoShk(t *testing.T) {
	var tests = []struct {
		modFile    string
		goldenFile string
	}{
		{
			modFile:    "./example-models/nonmem/73/IOVMM/10.mod",
			goldenFile: "./example-models/nonmem/73/IOVMM/IOVMM_table.txt",
		},
		{
			modFile:    "./example-models/nonmem/73/IOVMM_COV/10.mod",
			goldenFile: "./example-models/nonmem/73/IOVMM_COV/IOVMM_COV_table.txt",
		},
	}
	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, tt := range tests {
		context := filepath.Base(tt.goldenFile)

		// read bytes from golden txt
		goldenJSON, err := afero.ReadFile(osFs, tt.goldenFile)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] file error %s: %s", context, tt.goldenFile, err))

		// execute bbi and capture summary output as string
		stdout, err := exec.Command(bbiExe, "summary", tt.modFile, "--no-grd-file", "--no-shk-file").Output()
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail exec %s: %s", context, bbiExe, err))

		//compare file
		b := assert.ObjectsAreEqual(goldenJSON, stdout)
		assert.Equal(t, true, b, fmt.Sprintf("[%s] txt goldenfile not equal to table output", tt.goldenFile))
		if b == false {
			fmt.Printf("%s", string(stdout))
		}
	}
}
