package runner

import (
	"fmt"
)

// EstOutputFileCleanLevels gives a map of
// information about the output of NONMEM estimations
// to be used for cleaning and other manipulations
func EstOutputFileCleanLevels() map[string]int {
	var EstOutputFiles = make(map[string]int)
	EstOutputFiles["background.set"] = 1
	EstOutputFiles["compile.lnk"] = 1
	EstOutputFiles["FCON"] = 1
	EstOutputFiles["FDATA"] = 1
	EstOutputFiles["FMSG"] = 1
	EstOutputFiles["FREPORT"] = 1
	EstOutputFiles["FSIZES"] = 1
	EstOutputFiles["FSTREAM"] = 1
	EstOutputFiles["FSUBS"] = 1
	EstOutputFiles["FSUBS_MU.F90"] = 1
	EstOutputFiles["FSUBS.f90"] = 1
	EstOutputFiles["FSUBS2"] = 1
	EstOutputFiles["gfortran.txt"] = 1
	EstOutputFiles["INTER"] = 2
	EstOutputFiles["licfile.set"] = 1
	EstOutputFiles["LINK.LNK"] = 1
	EstOutputFiles["LINKC.LNK"] = 1
	EstOutputFiles["locfile.set"] = 1
	EstOutputFiles["maxlim.set"] = 1
	EstOutputFiles["newline"] = 1
	EstOutputFiles["nmexec.set"] = 1
	EstOutputFiles["nmpathlist.txt"] = 1
	EstOutputFiles["nmprd4p.mod"] = 1
	EstOutputFiles["nobuild.set"] = 1
	EstOutputFiles["nonmem"] = 1
	EstOutputFiles["parafile.set"] = 1
	EstOutputFiles["parafprint.set"] = 1
	EstOutputFiles["prcompile.set"] = 1
	EstOutputFiles["prdefault.set"] = 1
	EstOutputFiles["prsame.set"] = 1
	EstOutputFiles["PRSIZES.f90"] = 1
	EstOutputFiles["rundir.set"] = 1
	EstOutputFiles["runpdir.set"] = 1
	EstOutputFiles["simparon.set"] = 1
	EstOutputFiles["temp_dir"] = 1
	EstOutputFiles["tprdefault.set"] = 1
	EstOutputFiles["trskip.set"] = 1
	EstOutputFiles["worker.set"] = 1
	EstOutputFiles["xmloff.set"] = 1
	return EstOutputFiles
}

// EstOutputFilesByRun creates a map of run specific file names
// in the output directory
func EstOutputFilesByRun(r string) map[string]int {
	var EstOutputFiles = make(map[string]int)
	fileExtsLvl1 := []string{
		".clt",
		".coi",
		".clt",
		".coi",
		".cpu",
		".grd",
		".shk",
		".shm",
		".xml",
	}
	fileExtsLvl2 := []string{
		".cor",
		".cov",
		".ext",
		".lst",
		".phi",
	}
	for _, f := range fileExtsLvl1 {
		EstOutputFiles[fmt.Sprintf("%s%s", r, f)] = 1
	}
	for _, f := range fileExtsLvl2 {
		EstOutputFiles[fmt.Sprintf("%s%s", r, f)] = 2
	}
	return EstOutputFiles
}

//EstOutputFiles["sdtab001"] = 1
// EstOutputFiles["msf001"] = 1
// EstOutputFiles["msf001_ETAS"] = 1
// EstOutputFiles["msf001_RMAT"] = 1
// EstOutputFiles["msf001_SMAT"] = 1
// var OUTPUT_FILES = []string{
// 	"run001.clt"
// 	"run001.coi"
// 	"run001.cor"
// 	"run001.cov"
// 	"run001.cpu"
// 	"run001.ext"
// 	"run001.grd"
// 	"run001.lst"
// 	"run001.mod"
// 	"run001.phi"
// 	"run001.shk"
// 	"run001.shm"
// 	"run001.xml"
// }
