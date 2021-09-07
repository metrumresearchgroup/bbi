package parser

import "testing"

var thetaSlice01 = []string{
	"(0,2720) ; CL",
	"(0,2650) ; V2",
	"(0,7730) ; V3",
	"(0,3410)",
	"(0,0.232) FIX ; KA transit",
}

var thetaSlice01Formatted = []string{
	"(0,2720)      ; CL",
	"(0,2650)      ; V2",
	"(0,7730)      ; V3",
	"(0,3410)      ; <NEED COMMENT>",
	"(0,0.232) FIX ; KA transit",
}

var cleanedThetaBlock02 = []string{
	"(0,5) ; CL",
	"(0,97.2) ; V",
	"(0,7.47,100) ; Q",
	"(0,106,500) ; V2",
	"(-0.011,0.00977,0.030) ; CLWT1",
	"(-1,-0.16,5) ; V1SEX1",
	"(-0.011,0.0142,0.030) ; V1WT1",
	"(-1000000,0.0178,0.045) ; CLCRCL1",
	"(-0.038,0.00231,1000000) ; CLCRCL2",
}

var cleanedThetaBlock02Formatted = []string{
	"(0,5)                    ; CL",
	"(0,97.2)                 ; V",
	"(0,7.47,100)             ; Q",
	"(0,106,500)              ; V2",
	"(-0.011,0.00977,0.030)   ; CLWT1",
	"(-1,-0.16,5)             ; V1SEX1",
	"(-0.011,0.0142,0.030)    ; V1WT1",
	"(-1000000,0.0178,0.045)  ; CLCRCL1",
	"(-0.038,0.00231,1000000) ; CLCRCL2",
}

func TestFormattingThetaBlock(t *testing.T) {
	formattedData := FormatThetaBlock(thetaSlice01)
	for i, val := range formattedData {
		if val != thetaSlice01Formatted[i] {
			t.Log("GOT: ", val, " EXPECTED: ", thetaSlice01Formatted[i])
			t.Fail()
		}
	}
}

func TestFormattingThetaBlock02(t *testing.T) {
	formattedData := FormatThetaBlock(cleanedThetaBlock02)
	for i, val := range formattedData {
		if val != cleanedThetaBlock02Formatted[i] {
			t.Log("GOT: ", val, " EXPECTED: ", cleanedThetaBlock02Formatted[i])
			t.Fail()
		}
	}
}
