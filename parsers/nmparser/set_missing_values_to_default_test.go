package parser

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSetMissingValuesToDefaultParameterDataMethod(t *testing.T) {
	var tests = []struct {
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    string
	}{
		{
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{}},
			},
			expected: "METHOD NOT DETECTED",
		},
		{
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Method: "Test Method",
				}},
			},
			expected: "Test Method",
		},
	}
	for _, tt := range tests {
		setMissingValuesToDefault(&tt.modelOutput, tt.etaCount, tt.epsCount)
		assert.Equal(t, tt.expected, tt.modelOutput.ParametersData[0].Method, "Fail :"+tt.expected)
	}
}

func TestSetMissingValuesToDefaultParameterDataStdErrDimension(t *testing.T) {
	var tests = []struct {
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{
						Theta: []float64{1, 2, 4},
					},
				}},
			},
			etaCount: 3,
			expected: 3,
		},
	}

	for _, tt := range tests {
		setMissingValuesToDefault(&tt.modelOutput, tt.etaCount, tt.epsCount)
		assert.Equal(t, tt.expected, len(tt.modelOutput.ParametersData[0].StdErr.Theta), "Fail :"+tt.context)
		assert.NotEqual(t, tt.expected, len(tt.modelOutput.ParametersData[0].StdErr.Omega), "Fail :"+tt.context)
	}
}

func TestSetMissingValuesToDefaultParameterDataRESDDimension(t *testing.T) {
	var tests = []struct {
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{
						Omega: []float64{1, 2, 4},
					},
				}},
			},
			etaCount: 3,
			expected: 3,
		},
	}

	for _, tt := range tests {
		setMissingValuesToDefault(&tt.modelOutput, tt.etaCount, tt.epsCount)
		assert.Equal(t, tt.expected, len(tt.modelOutput.ParametersData[0].RandomEffectSD.Omega), "Fail :"+tt.context)
		assert.NotEqual(t, tt.expected, len(tt.modelOutput.ParametersData[0].RandomEffectSD.Sigma), "Fail :"+tt.context)
	}
}

func TestSetMissingValuesToDefaultParameterDataValues(t *testing.T) {
	var tests = []struct {
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{
						Omega: []float64{1, 2, 4},
						Sigma: []float64{5, 6},
						Theta: []float64{7, 8, 9, 10, 11, 12, 13},
					},
				}},
			},
			etaCount: 3,
		},
	}

	for _, tt := range tests {
		setMissingValuesToDefault(&tt.modelOutput, tt.etaCount, tt.epsCount)
		assert.Equal(t, 7, len(tt.modelOutput.ParametersData[0].StdErr.Theta), "Fail :"+tt.context)
		assert.Equal(t, 2, len(tt.modelOutput.ParametersData[0].RandomEffectSD.Sigma), "Fail :"+tt.context)
		assert.Equal(t, 3, len(tt.modelOutput.ParametersData[0].RandomEffectSDSE.Omega), "Fail :"+tt.context)

		assert.Equal(t, DefaultFloat64, tt.modelOutput.ParametersData[0].StdErr.Theta[0], "Fail :"+tt.context)
		assert.Equal(t, DefaultFloat64, tt.modelOutput.ParametersData[0].RandomEffectSD.Sigma[1], "Fail :"+tt.context)
		assert.Equal(t, DefaultFloat64, tt.modelOutput.ParametersData[0].RandomEffectSDSE.Omega[2], "Fail :"+tt.context)
	}
}

func TestSetMissingValuesToDefaultParameterNameValues(t *testing.T) {
	var tests = []struct {
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{
						Theta: []float64{1, 2, 4},
					},
				}},
			},
			etaCount: 3,
			epsCount: 3,
		},
	}

	for _, tt := range tests {
		setMissingValuesToDefault(&tt.modelOutput, tt.etaCount, tt.epsCount)
		for i := range tt.modelOutput.ParametersData[0].Estimates.Theta {
			assert.Equal(t, fmt.Sprintf("THETA%d", i+1), tt.modelOutput.ParameterNames.Theta[i], "Fail :"+tt.context)
		}
	}
}

func TestSetMissingValuesToDefaultShrinkageEta(t *testing.T) {
	var tests = []struct {
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{},
				}},
				ShrinkageDetails: [][]ShrinkageDetails{{ShrinkageDetails{}}},
			},
			etaCount: 5,
		},
	}

	for _, tt := range tests {
		setMissingValuesToDefault(&tt.modelOutput, tt.etaCount, tt.epsCount)
		assert.Equal(t, tt.etaCount, len(tt.modelOutput.ShrinkageDetails[0][0].EtaBar), "Fail :"+tt.context)
		assert.Equal(t, DefaultFloat64, tt.modelOutput.ShrinkageDetails[0][0].EtaBar[tt.etaCount-1], "Fail :"+tt.context)
	}
}

func TestSetMissingValuesToDefaultShrinkageEps(t *testing.T) {
	var tests = []struct {
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{},
				}},
				ShrinkageDetails: [][]ShrinkageDetails{{ShrinkageDetails{}}},
			},
			epsCount: 5,
		},
	}

	for _, tt := range tests {
		setMissingValuesToDefault(&tt.modelOutput, tt.etaCount, tt.epsCount)
		assert.Equal(t, tt.epsCount, len(tt.modelOutput.ShrinkageDetails[0][0].EpsVR), "Fail :"+tt.context)
		assert.Equal(t, DefaultFloat64, tt.modelOutput.ShrinkageDetails[0][0].EpsVR[tt.epsCount-1], "Fail :"+tt.context)
	}
}
