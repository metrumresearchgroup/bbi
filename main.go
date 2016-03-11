package main

import (
	"fmt"

	"github.com/dpastoor/go-nm-utils/nonmemutils"
)

func main() {
	// file, _ := ioutil.ReadFile("./test/fixtures/blocks/theta-block-01.lst")
	// // will need to check this also works as expected on windows and doesn't
	// // keep the \r as well, couldt ry something like runtime.GOOS == "windows"
	// newFile := strings.Split(string(file), "\n")
	// for i, f := range newFile {
	// 	fmt.Println(i, f)
	// }
	fmt.Println(nonmemutils.CleanThetaBlock([]string{"$THETA", "0 FIX"}))
}
