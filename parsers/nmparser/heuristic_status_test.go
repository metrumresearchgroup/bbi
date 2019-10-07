package parser

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestUnmarshalJSON(t *testing.T) {
	var tests = []struct {
		value    []byte
		expected HeuristicStatus
		context  string
	}{
		{
			value:    []byte(string(`"HeuristicUndefined"`)),
			expected: HeuristicUndefined,
			context:  "HeuristicUndefined",
		},
		{
			value:    []byte(string(`"HeuristicTrue"`)),
			expected: HeuristicTrue,
			context:  "HeuristicTrue",
		},
		{
			value:    []byte(string(`"HeuristicFalse"`)),
			expected: HeuristicFalse,
			context:  "HeuristicFalse",
		},
	}

	for _, tt := range tests {

		hs := new(HeuristicStatus)
		err := hs.UnmarshalJSON(tt.value)
		assert.Equal(t, nil, err, "Fail :"+tt.context)
		assert.Equal(t, tt.expected, *hs, "Fail :"+tt.context)
	}
}

func TestMarshalJSON(t *testing.T) {
	var tests = []struct {
		value    HeuristicStatus
		expected string
		context  string
	}{
		{
			value:    HeuristicUndefined,
			expected: `"HeuristicUndefined"`,
			context:  "HeuristicUndefined",
		},
		{
			value:    HeuristicTrue,
			expected: `"HeuristicTrue"`,
			context:  "HeuristicTrue",
		},
		{
			value:    HeuristicFalse,
			expected: `"HeuristicFalse"`,
			context:  "HeuristicFalse",
		},
	}

	for _, tt := range tests {
		b, err := tt.value.MarshalJSON()
		assert.Equal(t, nil, err, "Fail :"+tt.context)
		assert.Equal(t, tt.expected, string(b), "Fail :"+tt.context)
	}
}
