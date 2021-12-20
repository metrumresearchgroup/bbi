package parser

import (
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"
	"github.com/metrumresearchgroup/wrapt"
)

func TestParseParameterBlock(tt *testing.T) {
	input := []string{
		" 1",
		" 0  2",
		" 0  2  2",
		" 0  0  0  3",
	}
	expected := []int{1, 0, 2, 0, 2, 2, 0, 0, 0, 3}

	testId := "UNIT-NMP-008"
	tt.Run(utils.AddTestId("", testId), func(tt *testing.T) {
		t := wrapt.WrapT(tt)

		parsedData := ParseBlockStructure(input)
		t.R.Equal(expected, parsedData)
	})
}
