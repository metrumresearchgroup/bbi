package parser

// example omega initial estimates given complex omega structure
//
//	IOV = EXP(ETA(3))
//	IF(VISI.EQ.6) IOV=EXP(ETA(4))
//	IF(VISI.EQ.8) IOV=EXP(ETA(5))
//	$OMEGA  0.14  ; 1 IIV BASE
//	$OMEGA  0.8  ; 2 IIV EC50
//	$OMEGA  BLOCK(1)
//	 0.03  ; 1 IOV BASE
//	$OMEGA  BLOCK(1) SAME
//	$OMEGA  BLOCK(1) SAME
//
// TODO: remove omegaBlock01 if we find no use.
var _ = /*omegaBlock01*/ []string{
	"0INITIAL ESTIMATE OF OMEGA:",
	" BLOCK SET NO.   BLOCK                                                                    FIXED",
	"        1                                                                                   NO ",
	"                  0.1400E+00",
	"        2                                                                                   NO ",
	"                  0.8000E+00",
	"        3                                                                                   NO ",
	"                  0.3000E-01",
	"0INITIAL ESTIMATE OF SIGMA:",
}
