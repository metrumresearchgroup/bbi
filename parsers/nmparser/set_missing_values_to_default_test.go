package parser

import (
	"fmt"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestSetMissingValuesToDefaultParameterDataMethod(tt *testing.T) {
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
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			name: "test",
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
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			name: "test",
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
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			name: "test",
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

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)
			t.R.Equal(7, len(test.modelOutput.ParametersData[0].StdErr.Theta), "Fail :"+test.context)
			t.R.Equal(2, len(test.modelOutput.ParametersData[0].RandomEffectSD.Sigma), "Fail :"+test.context)
			t.R.Equal(3, len(test.modelOutput.ParametersData[0].RandomEffectSDSE.Omega), "Fail :"+test.context)

			t.R.Equal(DefaultFloat64, test.modelOutput.ParametersData[0].StdErr.Theta[0], "Fail :"+test.context)
			t.R.Equal(DefaultFloat64, test.modelOutput.ParametersData[0].RandomEffectSD.Sigma[1], "Fail :"+test.context)
			t.R.Equal(DefaultFloat64, test.modelOutput.ParametersData[0].RandomEffectSDSE.Omega[2], "Fail :"+test.context)
		})
	}
}

func TestSetMissingValuesToDefaultParameterNameValues(tt *testing.T) {
	var tests = []struct {
		name        string
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			name: "test",
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

func TestSetMissingValuesToDefaultShrinkageEta(tt *testing.T) {
	var tests = []struct {
		name        string
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			name: "test",
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{},
				}},
				ShrinkageDetails: [][]ShrinkageDetails{{ShrinkageDetails{}}},
			},
			etaCount: 5,
		},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)

			t.R.Equal(test.etaCount, len(test.modelOutput.ShrinkageDetails[0][0].EtaBar))
			t.R.Equal(DefaultFloat64, test.modelOutput.ShrinkageDetails[0][0].EtaBar[test.etaCount-1])
		})
	}
}

func TestSetMissingValuesToDefaultShrinkageEps(tt *testing.T) {
	var tests = []struct {
		name        string
		modelOutput SummaryOutput
		etaCount    int
		epsCount    int
		expected    int
		context     string
	}{
		{
			name: "test",
			modelOutput: SummaryOutput{
				ParametersData: []ParametersData{ParametersData{
					Estimates: ParametersResult{},
				}},
				ShrinkageDetails: [][]ShrinkageDetails{{ShrinkageDetails{}}},
			},
			epsCount: 5,
		},
	}

	for _, test := range tests {
		tt.Run(test.name, func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			setMissingValuesToDefault(&test.modelOutput)

			t.R.Equal(test.epsCount, len(test.modelOutput.ShrinkageDetails[0][0].EpsVR))
			t.R.Equal(DefaultFloat64, test.modelOutput.ShrinkageDetails[0][0].EpsVR[test.epsCount-1])
		})
	}
}
