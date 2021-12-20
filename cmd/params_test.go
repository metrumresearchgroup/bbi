package cmd

import (
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"
	"github.com/metrumresearchgroup/wrapt"
)

func TestSplitParam(tt *testing.T) {
	type expected struct {
		base     string
		subindex []int
	}

	tests := []struct {
		name     string
		input    string
		expected expected
	}{
		{
			name:     "scalar parameter",
			input:    "THETA",
			expected: expected{"THETA", nil},
		},
		{
			name:     "vector parameter",
			input:    "THETA1",
			expected: expected{"THETA", []int{1}},
		},
		{
			name:     "matrix parameter",
			input:    "THETA(1,1)",
			expected: expected{"THETA", []int{1, 1}},
		},
		{
			name:     "all delim",
			input:    "1234",
			expected: expected{"1234", nil},
		},
		{
			name:     "delim prefix",
			input:    "12foo34",
			expected: expected{"12foo", []int{34}},
		},
		{
			name:     "invalid index",
			input:    "foo34bar",
			expected: expected{"foo34bar", nil},
		},
	}

	id := "UNIT-PARAM-001"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, id), func(tt *testing.T) {
			t := wrapt.WrapT(tt)
			base, subindex := splitParam(test.input)
			t.R.Equal(test.expected.base, base)
			t.R.Equal(test.expected.subindex, subindex)
		})
	}
}

func TestSortParam(tt *testing.T) {
	tests := []struct {
		name     string
		input    []string
		expected []string
	}{
		{
			name:     "01",
			input:    []string{"THETA", "ALPHA", "BETA"},
			expected: []string{"THETA", "ALPHA", "BETA"},
		},
		{
			name: "02",
			input: []string{
				"THETA(1,1)", "ALPHA1", "BETA",
				"ALPHA2", "THETA(1,2)"},
			expected: []string{
				"THETA(1,1)", "THETA(1,2)",
				"ALPHA1", "ALPHA2",
				"BETA"},
		},
		{
			name: "03",
			input: []string{
				"THETA(2,1)", "THETA(1,1)", "ALPHA2", "BETA1",
				"ALPHA1", "THETA(1,2)", "BETA", "THETA"},
			expected: []string{
				"THETA", "THETA(1,1)", "THETA(1,2)", "THETA(2,1)",
				"ALPHA1", "ALPHA2",
				"BETA", "BETA1"},
		},
	}

	id := "UNIT-PARAM-002"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, id), func(tt *testing.T) {
			t := wrapt.WrapT(tt)
			t.R.Equal(test.expected, sortParams(test.input))
		})
	}
}
