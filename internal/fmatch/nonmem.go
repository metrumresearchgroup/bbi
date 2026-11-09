// Copyright 2026 Metrum Research Group
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package fmatch

import (
	"github.com/metrumresearchgroup/bbi/configlib"
)

// NonmemCleanMatcher returns Rules for matching files in a NONMEM output
// directory that should be deleted.  cfg is the configuration used for to run
// the model.
func NonmemCleanMatcher(cfg configlib.Config) Rules {
	rs := []Rule{
		{Pattern: "background.set", Level: 1},
		{Pattern: "compile.lnk", Level: 1},
		{Pattern: "condor.set", Level: 1},
		{Pattern: "condorarguments.set", Level: 1},
		{Pattern: "condoropenmpiscript.set", Level: 1},
		{Pattern: "FCON", Level: 1},
		{Pattern: "FDATA", Level: 1},
		{Pattern: "FDATA.csv", Level: 1},
		{Pattern: "flushtime.set", Level: 1},
		{Pattern: "FMSG", Level: 1},
		{Pattern: "fort.2001", Level: 1},
		{Pattern: "fort.2002", Level: 1},
		{Pattern: "FPWARN", Level: 1},
		{Pattern: "FREPORT", Level: 1},
		{Pattern: "FSIZES", Level: 1},
		{Pattern: "FSTREAM", Level: 1},
		{Pattern: "FSUBS", Level: 1},
		{Pattern: "FSUBS.0", Level: 1},
		{Pattern: "FSUBS.f90", Level: 1},
		{Pattern: "fsubs.f90", Level: 1},
		{Pattern: "FSUBS.o", Level: 1},
		{Pattern: "FSUBS2", Level: 1},
		{Pattern: "FSUBS_MU.F90", Level: 1},
		{Pattern: "GFCOMPILE.BAT", Level: 1},
		{Pattern: "gfortran.txt", Level: 1},
		{Pattern: "INTER", Level: 1},
		{Pattern: "licfile.set", Level: 1},
		{Pattern: "LINK.LNK", Level: 1},
		{Pattern: "linkc.lnk", Level: 1},
		{Pattern: "LINKC.LNK", Level: 1},
		{Pattern: "locfile.set", Level: 1},
		{Pattern: "maxlim.set", Level: 1},
		{Pattern: "mpiloc", Level: 1},
		{Pattern: "newline", Level: 1},
		{Pattern: "nmexec.set", Level: 1},
		{Pattern: "nmmpi.sh", Level: 1},
		{Pattern: "nmpathlist.txt", Level: 1},
		{Pattern: "nmprd4p.mod", Level: 1},
		{Pattern: "nobuild.set", Level: 1},
		{Pattern: "nonmem", Level: 1},
		{Pattern: "nonmem.exe", Level: 1},
		{Pattern: "parafile.set", Level: 1},
		{Pattern: "parafprint.set", Level: 1},
		{Pattern: "prcompile.set", Level: 1},
		{Pattern: "prdefault.set", Level: 1},
		{Pattern: "prsame.set", Level: 1},
		{Pattern: "PRSIZES.f90", Level: 1},
		{Pattern: "rundir.set", Level: 1},
		{Pattern: "runpdir.set", Level: 1},
		{Pattern: "simparon.set", Level: 1},
		{Pattern: "temp.out", Level: 1},
		{Pattern: "temp_dir", Level: 1},
		{Pattern: "tprdefault.set", Level: 1},
		{Pattern: "trashfile.xxx", Level: 1},
		{Pattern: "trskip.set", Level: 1},
		{Pattern: "worker.set", Level: 1},
		{Pattern: "xmloff.set", Level: 1},
	}

	if cfg.Parallel {
		rs = append(rs, []Rule{
			{Pattern: "fort.[0-9]*", Glob: true, Level: 1},
			{Pattern: "WK_[0-9]*", Glob: true, Level: 1, OnlyEmpty: true},
			{Pattern: "worker[0-9]*", Glob: true, Level: 1},
		}...)
	}

	return NewRules(rs)
}
