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
		results = val.(string)
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
		omegaRows := val.(map[string]interface{})
		for _, omegaRow := range omegaRows {
			vals, ok := omegaRow.([]interface{})
			if !ok {
				// TODO: solve omegaRow !ok
			}
			for _, omegaVal := range vals {
				rowVals, ok := omegaVal.(map[string]interface{})
				if !ok {
					// TODO: solve omegaVal !ok
				}
				colMap := rowVals["col"]
				if rowVals["-rname"] == "1" || rowVals["-rname"] == "THETA1" {
					colVals, ok := colMap.(map[string]interface{})
					// TODO: solve colMap !ok
					results = append(results, blockValue{
						Value: colVals["#text"].(string),
						Row:   rowVals["-rname"].(string),
						Col:   colVals["-cname"].(string),
					})
				} else {
					for _, m := range colMap.([]interface{}) {
						colVals, ok := m.(map[string]interface{})
						if !ok {
							// TODO: solve m !ok
						}
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
			thetaVals, ok := v.([]interface{})
			if !ok {
				// TODO: solve thetaVals !ok
			}
			for _, tv := range thetaVals {
				vals, ok := tv.(map[string]interface{})
				if !ok {
					// TODO: solve tv !ok
				}
				output = append(output, vals["#text"].(string))
			}
		}
	}

	return output
}
