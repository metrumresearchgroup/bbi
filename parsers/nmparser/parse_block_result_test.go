package parser

import (
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/wrapt"
)

func TestParseBlockResults(tt *testing.T) {
	tests := []struct {
		name     string
		input    []string
		expected []float64
	}{
		{
			name: "block",
			input: []string{
				"            ETA1      ETA2",
				"",
				" ETA1",
				"+        1.23E-01",
				"",
				" ETA2",
				"+        0.00E+00  1.54E-01",
			},
			expected: []float64{
				0.123,
				0,
				0.154,
			},
		},
		{
			name: "with dots",
			input: []string{
				"            ETA1      ETA2",
				"",
				" ETA1",
				"+        1.23E-01",
				"",
				" ETA2",
				"+        .........  1.54E-01",
			},
			expected: []float64{
				0.123,
				DefaultFloat64,
				0.154,
			},
		},
	}

	testId := "UNIT-NMP-007"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			parsedData := ParseBlockResults(test.input)

			t.R.Equal(test.expected, parsedData)
		})
	}
}
