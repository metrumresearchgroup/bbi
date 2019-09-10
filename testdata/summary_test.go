package cmd

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"path/filepath"
	"testing"

	parser "github.com/babylon/parsers/nmparser"
	"github.com/spf13/afero"
	"github.com/stretchr/testify/assert"
)

func TestSummary(t *testing.T) {
	var tests = []struct {
		modFile    string
		goldenFile string
	}{
		{
			modFile:    "./example-models/nonmem/BQL/2.mod",
			goldenFile: "./example-models/nonmem/BQL/BQL.json",
		},
		{
			modFile:    "./example-models/nonmem/IOVMM/10.mod",
			goldenFile: "./example-models/nonmem/IOVMM/IOVMM.json",
		},
		{
			modFile:    "./example-models/nonmem/TMDD/1.mod",
			goldenFile: "./example-models/nonmem/TMDD/TMDD.json",
		},
		{
			modFile:    "./example-models/nonmem/NonLinearCL/1.mod",
			goldenFile: "./example-models/nonmem/NonLinearCL/NonLinearCL.json",
		},
	}
	bbiExe := "/usr/local/bin/bbi"
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

		// marshal golden objects to bytes
		goldenParametersData, err := json.Marshal(goldenSummary.ParametersData)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal golden ParametersData: %s", context, err))

		goldenParametersStructures, err := json.Marshal(goldenSummary.ParameterStructures)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal golden ParameterStructures: %s", context, err))

		goldenParametersNames, err := json.Marshal(goldenSummary.ParameterNames)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal golden ParameterNames: %s", context, err))

		goldenOfv, err := json.Marshal(goldenSummary.OFV)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal golden OFV: %s", context, err))

		goldenShrinkageDetails, err := json.Marshal(goldenSummary.ShrinkageDetails)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal golden ShrinkageDetails: %s", context, err))

		// execute bbi and capture summary output as string
		stdout, err := exec.Command(bbiExe, "summary", "--tree", tt.modFile).Output()
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail exec %s: %s", context, bbiExe, err))

		// unmarshal summary output
		var bbiSummary parser.ModelOutput
		err = json.Unmarshal(stdout, &bbiSummary)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to unmarshal summary: %s", context, err))

		// marshal summary data to bytes and compare to golden data
		summaryParametersData, err := json.Marshal(bbiSummary.ParametersData)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal summary ParametersData: %s", context, err))
		assert.Equal(t, goldenParametersData, summaryParametersData, fmt.Sprintf("[%s] parametersData not equal", context))

		summaryParametersStructures, err := json.Marshal(bbiSummary.ParameterStructures)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal summary ParameterStructures: %s", context, err))
		assert.Equal(t, goldenParametersStructures, summaryParametersStructures, fmt.Sprintf("[%s] parametersStructures not equal", context))

		summaryParametersNames, err := json.Marshal(bbiSummary.ParameterNames)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal summary ParameterNames: %s", context, err))
		assert.Equal(t, goldenParametersNames, summaryParametersNames, fmt.Sprintf("[%s] parametersStructures not equal", context))

		summaryOfv, err := json.Marshal(bbiSummary.OFV)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal summary OFV: %s", context, err))
		assert.Equal(t, goldenOfv, summaryOfv, fmt.Sprintf("[%s] parametersStructures not equal", context))

		summaryShrinkageDetails, err := json.Marshal(bbiSummary.ShrinkageDetails)
		assert.Equal(t, nil, err, fmt.Sprintf("[%s] fail to marshal summary ShrinkageDetails: %s", context, err))
		assert.Equal(t, goldenShrinkageDetails, summaryShrinkageDetails, fmt.Sprintf("[%s] parametersStructures not equal", context))

	}
}
