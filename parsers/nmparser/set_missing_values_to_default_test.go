package parser

import (
	"fmt"
	"github.com/metrumresearchgroup/bbi/utils"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestSetMissingValuesToDefaultParameterDataMethod(tt *testing.T) {
	var tests = []struct {
		modelOutput SummaryOutput
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
	testId := "UNIT-NMP-001"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.expected, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)

			t.R.Equal(test.expected, test.modelOutput.ParametersData[0].Method)
		})
	}
}

func TestSetMissingValuesToDefaultParameterDataStdErrDimension(tt *testing.T) {
	var tests = []struct {
		name        string
		modelOutput SummaryOutput
		expected    int
	}{
		{
			name: "3 thetas and 3 omegas",
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{
						Theta: []float64{1, 2, 4},
						Omega: []float64{1, 2, 4},
					},
				}},
			},
			expected: 3,
		},
	}

	testId := "UNIT-NMP-002"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)
			t.R.Equal(test.expected, len(test.modelOutput.ParametersData[0].StdErr.Theta))
			t.R.Equal(test.expected, len(test.modelOutput.ParametersData[0].StdErr.Omega))
		})
	}
}

func TestSetMissingValuesToDefaultParameterDataRESDDimension(tt *testing.T) {
	var tests = []struct {
		name        string
		modelOutput SummaryOutput
		expected    int
	}{
		{
			name: "3 omegas",
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{
						Omega: []float64{1, 2, 4},
					},
				}},
			},
			expected: 3,
		},
	}

	testId := "UNIT-NMP-003"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)
			t.R.Equal(test.expected, len(test.modelOutput.ParametersData[0].RandomEffectSD.Omega))
			t.R.NotEqual(test.expected, len(test.modelOutput.ParametersData[0].RandomEffectSD.Sigma))
		})
	}
}

func TestSetMissingValuesToDefaultParameterDataValues(tt *testing.T) {
	var tests = []struct {
		name        string
		modelOutput SummaryOutput
		expected    int
	}{
		{
			name: "7 thetas, 3 omegas, 2 sigmas",
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{
						Omega: []float64{1, 2, 4},
						Sigma: []float64{5, 6},
						Theta: []float64{7, 8, 9, 10, 11, 12, 13},
					},
				}},
			},
		},
	}

	testId := "UNIT-NMP-004"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)
			t.R.Equal(7, len(test.modelOutput.ParametersData[0].StdErr.Theta))
			t.R.Equal(2, len(test.modelOutput.ParametersData[0].RandomEffectSD.Sigma))
			t.R.Equal(3, len(test.modelOutput.ParametersData[0].RandomEffectSDSE.Omega))

			t.R.Equal(DefaultFloat64, test.modelOutput.ParametersData[0].StdErr.Theta[0])
			t.R.Equal(DefaultFloat64, test.modelOutput.ParametersData[0].RandomEffectSD.Sigma[1])
			t.R.Equal(DefaultFloat64, test.modelOutput.ParametersData[0].RandomEffectSDSE.Omega[2])
		})
	}
}

func TestSetMissingValuesToDefaultParameterNameValues(tt *testing.T) {
	var tests = []struct {
		name        string
		modelOutput SummaryOutput
		expected    int
	}{
		{
			name: "3 thetas",
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{
						Theta: []float64{1, 2, 4},
					},
				}},
			},
		},
	}

	testId := "UNIT-NMP-005"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)
			for i := range test.modelOutput.ParametersData[0].Estimates.Theta {
				t.R.Equal(fmt.Sprintf("THETA%d", i+1), test.modelOutput.ParameterNames.Theta[i])
			}
		})
	}
}
