package main

import (
	"fmt"
	"io/ioutil"

	"bytes"

	"github.com/clbanning/mxj"
)

func main() {
	xmlDataRaw, err := ioutil.ReadFile("run001.xml")
	if err != nil {
		fmt.Println("error reading")
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

	getThetas(&m)
	getBlockValues(&m, "omega")
	getBlockValues(&m, "sigma")
	//getBlockValues(&m, "covariance")
	//	fmt.Println(values)
}

func getBlockValues(m *mxj.Map, key string) {
	omegaKey := m.PathsForKey(key)
	value, err := m.ValuesForPath(omegaKey[0]) // should only have 1 place for thetas (for now?)
	// this could be different if multiple estimation steps run
	if err != nil {
		fmt.Println("error extracting values: ", err)
	}
	for _, val := range value {
		omegaRows := val.(map[string]interface{})
		for _, val := range omegaRows {
			vals := val.([]interface{})
			for _, val := range vals {
				rowVals := val.(map[string]interface{})
				colMap := rowVals["col"]
				if rowVals["-rname"] == "1" || rowVals["-rname"] == "THETA1" {
					colVals := colMap.(map[string]interface{})
					fmt.Println(rowVals["-rname"], colVals["-cname"])
					fmt.Println("value: ", colVals["#text"])
				} else {
					for _, m := range colMap.([]interface{}) {
						colVals := m.(map[string]interface{})
						fmt.Println(rowVals["-rname"], colVals["-cname"])
						fmt.Println("value: ", colVals["#text"])

						//fmt.Println(i, m["-cname"], m["#text"])
					}

				}
			}
		}
	}
}
func getThetas(m *mxj.Map) {
	thetaKey := m.PathsForKey("theta")
	values, err := m.ValuesForPath(thetaKey[0]) // should only have 1 place for thetas (for now?)
	// this could be different if multiple estimation steps run
	if err != nil {
		fmt.Println("error extracting values: ", err)
	}
	for _, val := range values {
		for _, val := range val.(map[string]interface{}) {
			// You'd probably want to process the value, as appropriate.
			// Here we just print it out.
			thetaVals := val.([]interface{})
			for _, val := range thetaVals {
				vals := val.(map[string]interface{})
				fmt.Println(vals["-name"])
				fmt.Println(vals["#text"])
			}
		}
	}

}
