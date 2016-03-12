package main

import (
	"fmt"

	"github.com/dpastoor/nonmemutils/nonmemutils"
)

func main() {

	omegaResult01 := []string{
		"            ETA1      ETA2",
		"",
		" ETA1",
		"+        1.23E-01",
		"",
		" ETA2",
		"+        0.00E+00  1.54E-01",
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

	fmt.Println(nonmemutils.ParseOmegaResults(omegaResult01))
}
