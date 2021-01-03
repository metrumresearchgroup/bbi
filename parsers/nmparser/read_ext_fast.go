package parser

import (
	"bufio"
	"errors"
	"os"
	"strings"
)

// ParseEstimatesFromExt parses out the estimates, parameter names, and estimate values
// from an ext file
func ParseEstimatesFromExt(file string) (ExtFastData, error) {
	fl, err := os.Open(file)
	if err != nil {
		return ExtFastData{}, err
	}
	defer fl.Close()
	fstat, err := fl.Stat()
	if err != nil {
		return ExtFastData{}, err
	}
	if fstat.Size() == 0 {
		return ExtFastData{}, errors.New("no ext file contents")
	}

	var estimationMethods []string
	estimationIndex := -1
	var paramNames []string
	var estimationLines [][]string
	var estLineDetected bool
	scanner := bufio.NewScanner(fl)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "TABLE") {
			estimationMethods = append(estimationMethods, line)
			estimationIndex++
		} else if strings.HasPrefix(line, " ITER") {
			if len(paramNames) == 0 {
				fields := strings.Fields(line)
				paramNames = fields[1:len(fields)-1]

			}
			continue
		} else if strings.HasPrefix(line, "  -1000000000"){
			estLineDetected = true
			// all lines minus the first which is the -100000000 and last which is OBJ
			fields :=strings.Fields(line)
			estimationLines = append(estimationLines, fields[1:len(fields)-1])
		}
	}
	// ext file can be present but have no -1000000000 for example from a control stream generating chain values
	//119   │
	//120   │ $EST METHOD=CHAIN FILE=580.chn DELIM=,
	//121   │ NSAMPLE=4 CTYPE=0 ISAMPLE=3 DF=0
	//122   │ SEED=122234 RANMETHOD=2 IACCEPT=0.5
	//123   │
	//124   │
	if !estLineDetected {
		return ExtFastData{}, errors.New("no estimation output detected")
	}
	return ExtFastData{
		EstimationMethods: estimationMethods,
		ParameterNames:    paramNames,
		EstimationLines: estimationLines,
	}, nil
}

