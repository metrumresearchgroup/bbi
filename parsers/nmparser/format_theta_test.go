package parser

import (
	"testing"

	"github.com/metrumresearchgroup/bbi/utils"

	"github.com/metrumresearchgroup/wrapt"
)

func TestFormattingThetaBlock(tt *testing.T) {
	tests := []struct {
		name     string
		input    []string
		expected []string
	}{
		{
			name: "slice",
			input: []string{
				"(0,2720) ; CL",
				"(0,2650) ; V2",
				"(0,7730) ; V3",
				"(0,3410)",
				"(0,0.232) FIX ; KA transit",
			},
			expected: []string{
				"(0,2720)      ; CL",
				"(0,2650)      ; V2",
				"(0,7730)      ; V3",
				"(0,3410)      ; <NEED COMMENT>",
				"(0,0.232) FIX ; KA transit",
			},
		},
		{
			name: "block",
			input: []string{
				"(0,5) ; CL",
				"(0,97.2) ; V",
				"(0,7.47,100) ; Q",
				"(0,106,500) ; V2",
				"(-0.011,0.00977,0.030) ; CLWT1",
				"(-1,-0.16,5) ; V1SEX1",
				"(-0.011,0.0142,0.030) ; V1WT1",
				"(-1000000,0.0178,0.045) ; CLCRCL1",
				"(-0.038,0.00231,1000000) ; CLCRCL2",
			},
			expected: []string{
				"(0,5)                    ; CL",
				"(0,97.2)                 ; V",
				"(0,7.47,100)             ; Q",
				"(0,106,500)              ; V2",
				"(-0.011,0.00977,0.030)   ; CLWT1",
				"(-1,-0.16,5)             ; V1SEX1",
				"(-0.011,0.0142,0.030)    ; V1WT1",
				"(-1000000,0.0178,0.045)  ; CLCRCL1",
				"(-0.038,0.00231,1000000) ; CLCRCL2",
			},
		},
	}
	testId := "UNIT-NMP-009"
	for _, test := range tests {
		tt.Run(utils.AddTestId(test.name, testId), func(tt *testing.T) {
			t := wrapt.WrapT(tt)

			got := FormatThetaBlock(test.input)

			t.R.Equal(test.expected, got)
		})
	}
}
