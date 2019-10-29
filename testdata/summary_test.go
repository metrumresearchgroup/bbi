package cmd

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/spf13/afero"
	"github.com/stretchr/testify/assert"
)

var Versions = []string{"73", "74"}
var Exts = []string{".json", "_table.txt"}
var TestFolder = string("./example-models/nonmem/")
var Tests = []struct {
	modFile string
	args    []string
}{
	{
		modFile: "meropenem/1.mod",
	},
	{
		modFile: "BQL/2.mod",
	},
	{
		modFile: "TMDD/1.mod",
	},
	{
		modFile: "NonLinearCL/1.mod",
	},
	{
		modFile: "IOVMM/10.mod",
	},
	{
		modFile: "contpkpd/1.mod",
	},
}

func TestSummary(t *testing.T) {
	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, tt := range Tests {
		for _, version := range Versions {
			for _, ext := range Exts {
				name := filepath.Base(filepath.Dir(tt.modFile))
				goldenFile := filepath.Join(TestFolder, version, name, name+ext)
				modFile := filepath.Join(TestFolder, version, tt.modFile)

				// read bytes from golden json
				goldenJSON, err := afero.ReadFile(osFs, goldenFile)
				assert.Equal(t, nil, err, fmt.Sprintf("file error %s: %s", goldenFile, err))

				var args []string
				args = append(args, "summary")
				if ext == ".json" {
					args = append(args, "--json")
				}
				args = append(args, tt.args...)
				args = append(args, modFile)
				stdout, err := exec.Command(bbiExe, args...).Output()
				assert.Equal(t, nil, err, fmt.Sprintf("fail exec %s: %s", bbiExe, err))

				//compare files
				b := assert.ObjectsAreEqual(goldenJSON, stdout)
				assert.Equal(t, true, b, fmt.Sprintf("[%s] %s goldenfile not equal to output", goldenFile, ext))
			}
		}
	}
}

// func TestBuildGoldenFiles(t *testing.T) {
// 	bbiExe := "bbi"
// 	osFs := afero.NewOsFs()
// 	for _, tt := range Tests {
// 		for _, version := range Versions {
// 			for _, ext := range Exts {
// 				name := filepath.Base(filepath.Dir(tt.modFile))
// 				goldenFile := filepath.Join(TestFolder, version, name, name+ext)
// 				modFile := filepath.Join(TestFolder, version, tt.modFile)
// 				var args []string
// 				args = append(args, "summary")
// 				if ext == ".json" {
// 					args = append(args, "--json")
// 				}
// 				args = append(args, tt.args...)
// 				args = append(args, modFile)
// 				stdout, err := exec.Command(bbiExe, args...).Output()
// 				assert.Equal(t, nil, err, fmt.Sprintf("%s fail exec %s: %s", goldenFile, bbiExe, err))
// 				fo, err := osFs.Create(goldenFile)
// 				assert.Equal(t, nil, err, fmt.Sprintf("%s fail open %s", goldenFile, err))
// 				_, err = fo.Write(stdout)
// 				assert.Equal(t, nil, err, fmt.Sprintf("%s fail write %s", goldenFile, err))
// 				err = fo.Close()
// 				assert.Equal(t, nil, err, fmt.Sprintf("%s fail close %s", goldenFile, err))
// 			}
// 		}
// 	}
// }
