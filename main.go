package main

import (
	"fmt"

	"github.com/dpastoor/nonmemutils/parser"
)

func main() {

	finalParameterEstimates01 := []string{
		"********************                             FINAL PARAMETER ESTIMATE                           ********************",
		" ********************                                                                                ********************",
		" ************************************************************************************************************************",
		" ",
		"",
		" THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********",
		"",
		"         TH 1      TH 2      TH 3      TH 4     ",
		" ",
		"         4.79E+00  9.02E+01  7.47E+00  1.05E+02",
		" ",
		"",
		" OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********",
		"",
		"            ETA1      ETA2   ",
		" ",
		" ETA1",
		"+        1.58E-01",
		" ",
		" ETA2",
		"+        1.22E-01  1.33E-01",
		" ",
		"",
		" SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****",
		"",
		"            EPS1      EPS2   ",
		" ",
		" EPS1",
		"+        1.45E+03",
		" ",
		" EPS2",
		"+        0.00E+00  7.39E-03",
		" ",
		"1",
		"",
		" OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******",
		"",
		"",
		"            ETA1      ETA2   ",
		" ",
		" ETA1",
		"+        3.98E-01",
		" ",
		" ETA2",
		"+        8.42E-01  3.65E-01",
		" ",
		"",
		" SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***",
		"",
		"            EPS1      EPS2   ",
		" ",
		" EPS1",
		"+        3.81E+01",
		" ",
		" EPS2",
		"+        0.00E+00  8.60E-02",
		" ",
		"1",
	}
	// uglyString := []string{
	// 	"$THETA (0,2720) ; CL",
	// 	"$THETA(0,2650) ; V2",
	// 	" $THETA (0,7730) ; V3",
	// 	" (0,3410)",
	// 	" $THETA(0,0.232) FIX ; KA transit",
	// 	" 0.75 FIX ; allo-WT",
	// }
	// //cleanedString := nonmemutils.CleanThetaBlock([]string{"$THETA", "0 FIX ; bad param", "1 ; CL", "2"})
	// cleanedString := nonmemutils.CleanThetaBlock(uglyString)
	// formattedString := nonmemutils.FormatThetaBlock(cleanedString)
	// for _, line := range uglyString {
	// 	fmt.Println(line)
	// }
	//
	// fmt.Println(" ")
	// fmt.Println("---------- AFTER CLEANING ---------")
	// fmt.Println(" ")
	// fmt.Println("$THETA")
	// for i := range cleanedString {
	// 	fmt.Println(formattedString[i])
	// }
	fmt.Println("parsed Final parameter estimates")
	fmt.Println(parser.ParseFinalParameterEstimates(finalParameterEstimates01))
}
