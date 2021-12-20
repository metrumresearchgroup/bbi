package main

import (
	"fmt"
	"io/ioutil"

	"bytes"

	"os"
	"path/filepath"

	"github.com/clbanning/mxj"
)

func main() {
	file, err := filepath.Abs(os.Args[1])
	fmt.Println(file)

	if os.IsNotExist(err) {
		fmt.Println("could not find specified file or resolve path")
	}
	xmlDataRaw, err := ioutil.ReadFile(file)
	if err != nil {
		fmt.Println("error reading")
		fmt.Println(err)
	}
	// we need to strip the ascii alert at the top of the xml file, as xml's parsers choke on it
	// and we don't have funky characters to need to worry about being specific about what
	// charset to use
	s := bytes.SplitAfterN(xmlDataRaw, []byte("\n"), 2)
	if err != nil {
		fmt.Println("error reading")
	}

	// all data after first line
	xmlData := s[1]
	m, merr := mxj.NewMapXml(xmlData)
	if merr != nil {
		fmt.Println("error mapping")
	}

	thetas := getThetas(&m, "theta")
	thetasSE := getThetas(&m, "thetase")
	omegas := getBlockValues(&m, "omega")
	sigmas := getBlockValues(&m, "sigma")
	omegasSE := getBlockValues(&m, "omegase")
	sigmasSE := getBlockValues(&m, "sigmase")
	// getBlockValues(&m, "covariance")
	//	fmt.Println(values)
	fmt.Println("thetas: ", thetas)
	fmt.Println("thetasSE: ", thetasSE)
	fmt.Println("omegas: ", omegas)
	fmt.Println("omegaSE: ", omegasSE)
	fmt.Println("sigmas: ", sigmas)
	fmt.Println("sigmasSE: ", sigmasSE)
	terminationStatus := getValue(&m, "termination_information")
	fmt.Println(terminationStatus)
}

type blockValue struct {
	Value string
	Row   string
	Col   string
}

func getValue(m *mxj.Map, key string) string {
	var results string
	keyPath := m.PathsForKey(key)
	value, err := m.ValuesForPath(keyPath[0])
	if err != nil {
		fmt.Println("err parsing: ", err)
	}
	for _, val := range value {
		// TODO: fix val !ok
		results, _ = val.(string)
	}

	return results
}
func getBlockValues(m *mxj.Map, key string) []blockValue {
	var results []blockValue
	omegaKey := m.PathsForKey(key)
	value, err := m.ValuesForPath(omegaKey[0]) // should only have 1 place for thetas (for now?)
	// this could be different if multiple estimation steps run
	if err != nil {
		fmt.Println("error extracting values: ", err)

		return results
	}
	for _, val := range value {
		// TODO: solve val !ok
		omegaRows, _ := val.(map[string]interface{})
		for _, omegaRow := range omegaRows {
			// TODO: solve omegaRow !ok
			vals, _ := omegaRow.([]interface{})
			for _, omegaVal := range vals {
				// TODO: solve omegaVal !ok
				rowVals, _ := omegaVal.(map[string]interface{})
				colMap := rowVals["col"]
				if rowVals["-rname"] == "1" || rowVals["-rname"] == "THETA1" {
					// TODO: solve colMap !ok
					colVals, _ := colMap.(map[string]interface{})
					results = append(results, blockValue{
						Value: colVals["#text"].(string),
						Row:   rowVals["-rname"].(string),
						Col:   colVals["-cname"].(string),
					})
				} else {
					for _, m := range colMap.([]interface{}) {
						// TODO: solve m !ok
						colVals, _ := m.(map[string]interface{})
						results = append(results, blockValue{
							Value: colVals["#text"].(string),
							Row:   rowVals["-rname"].(string),
							Col:   colVals["-cname"].(string),
						})
						// fmt.Println(i, m["-cname"], m["#text"])
					}
				}
			}
		}
	}

	return results
}
func getThetas(m *mxj.Map, key string) []string {
	var output []string
	thetaKey := m.PathsForKey(key)
	values, err := m.ValuesForPath(thetaKey[0]) // should only have 1 place for thetas (for now?)
	// this could be different if multiple estimation steps run
	if err != nil {
		fmt.Println("error extracting values: ", err)

		return output
	}
	for _, val := range values {
		for _, v := range val.(map[string]interface{}) {
			// You'd probably want to process the value, as appropriate.
			// Here we just print it out.
			// TODO: solve thetaVals !ok
			thetaVals, _ := v.([]interface{})
			for _, tv := range thetaVals {
				// TODO: solve tv !ok
				vals, _ := tv.(map[string]interface{})
				output = append(output, vals["#text"].(string))
			}
		}
	}

	return output
}
