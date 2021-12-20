package utils

import (
	"testing"

	"github.com/metrumresearchgroup/wrapt"
)

func TestListModelFiles(tt *testing.T) {
	tests := []struct {
		name  string
		input []string
		want  []string
	}{
		{
			name: "test",
			input: []string{
				"run001.mod",
				"run002.lst",
				"run003.mod",
				"nota.run",
			},
			want: []string{
				"run001.mod",
				"run003.mod",
			},
		},
	}

	testId := "UNIT-UTL-006"
	for _, test := range tests {
		tt.Run(AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got := ListFilesByExt(test.input, ".mod")

			t.R.Equal(test.want, got)
		})
	}
}
