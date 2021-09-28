package parser

import (
	"fmt"
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
	for _, test := range tests {
		tt.Run(test.expected, func(tt *testing.T) {
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

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
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

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
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

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)
			t.R.Equal(7, len(test.modelOutput.ParametersData[0].StdErr.Theta), "Fail :"+test.name)
			t.R.Equal(2, len(test.modelOutput.ParametersData[0].RandomEffectSD.Sigma), "Fail :"+test.name)
			t.R.Equal(3, len(test.modelOutput.ParametersData[0].RandomEffectSDSE.Omega), "Fail :"+test.name)

			t.R.Equal(DefaultFloat64, test.modelOutput.ParametersData[0].StdErr.Theta[0], "Fail :"+test.name)
			t.R.Equal(DefaultFloat64, test.modelOutput.ParametersData[0].RandomEffectSD.Sigma[1], "Fail :"+test.name)
			t.R.Equal(DefaultFloat64, test.modelOutput.ParametersData[0].RandomEffectSDSE.Omega[2], "Fail :"+test.name)
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

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)
			for i := range test.modelOutput.ParametersData[0].Estimates.Theta {
				t.R.Equal(fmt.Sprintf("THETA%d", i+1), test.modelOutput.ParameterNames.Theta[i])
			}
		})
	}
}
