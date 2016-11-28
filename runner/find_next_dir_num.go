package runner

import (
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"github.com/dpastoor/nonmemutils/utils"
)

//NextDirSuggestion provides a struct for the recommended next
// directory name, and whether there should be a project reorganization
// based on directory modifications
type NextDirSuggestion struct {
	NextDirName string
	Reorg       bool
}

//FindNextEstDirNum provides the next dir num
func FindNextEstDirNum(modelFile string, dirNames []string, padding int) NextDirSuggestion {
	existingRunNums := []int{}
	modelNameEst := strings.Join([]string{
		filepath.Base(modelFile),
		"_est_",
	}, "")

	for _, dirName := range dirNames {
		if strings.Contains(dirName, modelNameEst) {
			runNumString := strings.TrimPrefix(dirName, modelNameEst)
			runNum, _ := strconv.Atoi(runNumString)
			existingRunNums = append(existingRunNums, runNum)
		}
	}
	if len(existingRunNums) == 0 {
		return NextDirSuggestion{strings.Join([]string{
			modelNameEst,
			utils.PadNum(1, padding),
		}, ""), false}

	}
	sortedRunNums := sort.IntSlice(existingRunNums)
	sort.Sort(sortedRunNums)

	return NextDirSuggestion{strings.Join([]string{
		modelNameEst,
		utils.PadNum(sortedRunNums[len(sortedRunNums)-1]+1, padding),
	}, ""), false}
}
