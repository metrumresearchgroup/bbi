package parser

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/metrumresearchgroup/wrapt"
	"github.com/spf13/afero"
)

func TestSummary(tt *testing.T) {
	var Versions = []string{"73", "74"}
	var Exts = []string{".json", "_table.txt"}
	var TestFolder = string("../../testdata/example-models/nonmem/")
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

	bbiExe := "bbi"
	osFs := afero.NewOsFs()
	for _, test := range Tests {
		tt.Run(test.modFile, func(tt *testing.T) {
			for _, version := range Versions {
				tt.Run(version, func(tt *testing.T) {
					for _, ext := range Exts {
						tt.Run(ext, func(tt *testing.T) {
							t := wrapt.WrapT(tt)

							name := filepath.Base(filepath.Dir(test.modFile))
							goldenFile := filepath.Join(TestFolder, version, name, name+ext)
							modFile := filepath.Join(TestFolder, version, test.modFile)

							// read bytes from golden json
							goldenJSON, err := afero.ReadFile(osFs, goldenFile)
							t.R.Equal(nil, err, fmt.Sprintf("file error %s: %s", goldenFile, err))

							var args []string
							args = append(args, "summary")
							if ext == ".json" {
								args = append(args, "--json")
							}
							args = append(args, test.args...)
							args = append(args, modFile)
							stdout, err := exec.Command(bbiExe, args...).Output()
							t.R.Equal(nil, err, fmt.Sprintf("fail exec %s: %s", bbiExe, err))

							// compare files
							t.R.Equal(goldenJSON, stdout, "golden mismatch")
						})
					}
				})
			}
		})
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
// 				t.R.Equal(nil, err, fmt.Sprintf("%s fail exec %s: %s", goldenFile, bbiExe, err))
// 				fo, err := osFs.Create(goldenFile)
// 				t.R.Equal(nil, err, fmt.Sprintf("%s fail open %s", goldenFile, err))
// 				_, err = fo.Write(stdout)
// 				t.R.Equal(nil, err, fmt.Sprintf("%s fail write %s", goldenFile, err))
// 				err = fo.Close()
// 				t.R.Equal(nil, err, fmt.Sprintf("%s fail close %s", goldenFile, err))
// 			}
// 		}
// 	}
// }
