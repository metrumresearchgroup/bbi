package runner

import (
	"fmt"
	"strings"
)

// EstOutputFileCleanLevels gives a map of
// information about the output of NONMEM estimations
// to be used for cleaning and other manipulations.
func EstOutputFileCleanLevels(r string) map[string]int {
	var EstOutputFiles = make(map[string]int)
	EstOutputFiles["background.set"] = 1
	EstOutputFiles["compile.lnk"] = 1
	EstOutputFiles["FCON"] = 1
	EstOutputFiles["FDATA"] = 1
	EstOutputFiles["FDATA.csv"] = 1
	EstOutputFiles["FMSG"] = 1
	EstOutputFiles["FREPORT"] = 1
	EstOutputFiles["FSIZES"] = 1
	EstOutputFiles["FSTREAM"] = 1
	EstOutputFiles["FSUBS"] = 1
	EstOutputFiles["FSUBS.0"] = 1
	EstOutputFiles["FSUBS.o"] = 1
	EstOutputFiles["FSUBS_MU.F90"] = 1
	EstOutputFiles["FSUBS.f90"] = 1
	EstOutputFiles["fsubs.f90"] = 1
	EstOutputFiles["FSUBS2"] = 1
	EstOutputFiles["gfortran.txt"] = 1
	EstOutputFiles["GFCOMPILE.BAT"] = 1
	EstOutputFiles["INTER"] = 1
	EstOutputFiles["licfile.set"] = 1
	EstOutputFiles["linkc.lnk"] = 1
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
	EstOutputFiles["nonmem.exe"] = 1
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
	EstOutputFiles["fort.2001"] = 1
	EstOutputFiles["fort.2002"] = 1
	EstOutputFiles["flushtime.set"] = 1

	msfFileSuffixes := []string{
		"",
		"_ETAS",
		"_RMAT",
		"_SMAT",
		".msf",
		"_ETAS.msf",
		"_RMAT.msf",
		"_SMAT.msf",
	}

	//Examples
	//For RUN001 as the identifier:
	//run001_ETAS -> msfb001_etas
	//run001_RMAT.msf -> msfb001_RMAT.mfs
	for _, f := range msfFileSuffixes {
		EstOutputFiles[strings.Replace(fmt.Sprintf("%s%s", r, f), "run", "msfb", 1)] = 1
	}

	return EstOutputFiles
}

// CleanFilesByRun sets clean levels for files by run.
func CleanFilesByRun(r string) map[string]int {
	var EstOutputFiles = make(map[string]int)
	// fileExtLvls are based on 1 = lowest priority -> n = highest priority

	return EstOutputFiles
}

// EstOutputFilesByRun creates a map of run specific file names
// in the output directory.
func EstOutputFilesByRun(r string) map[string]int {
	var EstOutputFiles = make(map[string]int)
	// fileExtLvls are based on 1 = lowest priority -> n = highest priority

	fileExtsLvl1 := []string{
		".msf",
		"_ETAS.msf",
		"_RMAT.msf",
	}
	fileExtsLvl2 := []string{
		".clt",
		".coi",
		".cpu",
		".shm",
		".phi",
	}

	// parser now needs all these files + other tooling uses xml files
	fileExtsLvl3 := []string{
		".xml",
		".grd",
		".shk",
		".cor",
		".cov",
		".ext",
		".lst",
	}

	msfFileSuffixes := []string{
		"",
		"_ETAS",
		"_RMAT",
		"_SMAT",
		".msf",
		"_ETAS.msf",
		"_RMAT.msf",
		"_SMAT.msf",
	}

	for _, f := range fileExtsLvl1 {
		EstOutputFiles[fmt.Sprintf("%s%s", r, f)] = 1
	}
	for _, f := range fileExtsLvl2 {
		EstOutputFiles[fmt.Sprintf("%s%s", r, f)] = 2
	}
	for _, f := range msfFileSuffixes {
		EstOutputFiles[strings.Replace(fmt.Sprintf("%s%s", r, f), "run", "msfb", 1)] = 2
	}
	for _, f := range fileExtsLvl3 {
		EstOutputFiles[fmt.Sprintf("%s%s", r, f)] = 3
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
