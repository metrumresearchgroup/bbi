## bbi nonmem clean

clean files and folders

### Synopsis


glob examples:
bbi clean *.mod // anything with extension .mod
bbi clean *.mod --noFolders // anything with extension .mod
bbi clean run* // anything starting with run
regular expression examples:

bbi clean ^run --regex // anything beginning with the letters run
bbi clean ^run -v --regex // print out files and folders that will be deleted
bbi clean ^run --filesOnly --regex // only remove matching files
bbi clean _est_ --dirsOnly --regex // only remove matching folders
bbi clean _est_ --dirsOnly --preview --regex // show what output would be if clean occured but don't actually clean
bbi clean "run009.[^mod]" --regex // all matching run009.<ext> but not .mod files
bbi clean "run009.(mod|lst)$" --regex // match run009.lst and run009.mod

can also clean via the opposite of a match with inverse

bbi clean ".modt{0,1}$" --filesOnly --inverse --regex // clean all files not matching .mod or .modt

clean copied files via

bbi clean --copiedRuns="run001"
bbi clean --copiedRuns="run[001:010]"

can be a comma separated list as well

bbi clean --copiedRuns="run[001:010],run100"
 

```
bbi nonmem clean [flags]
```

### Options

```
      --copiedRuns string   run names
      --dirsOnly            only match and clean directories
      --filesOnly           only match and clean files
  -h, --help                help for clean
      --inverse             inverse selection from the given regex match criteria
      --regex               use regular expression to match instead of glob
```

### Options inherited from parent commands

```
      --background             RAW NMFE OPTION - Tells NONMEM not to scan StdIn for control characters
  -d, --debug                  debug mode
      --json                   json tree of output, if possible
      --licfile string         RAW NMFE OPTION - Specify a license file to use with NMFE (NONMEM)
      --maxlim int             RAW NMFE OPTION - Set the maximum values for the buffers used by NONMEM (if 0, don't pass -maxlim to nmfe) (default 2)
      --mpi_exec_path string   The fully qualified path to mpiexec. Used for NONMEM parallel operations (default "/usr/local/mpich3/bin/mpiexec")
      --nm_version string      Version of NONMEM from the configuration list to use
      --nmqual                 Whether or not to execute with nmqual (autolog.pl)
      --nobuild                RAW NMFE OPTION - Skips recompiling and rebuilding on NONMEM executable
  -o, --output string          output file
      --parafile string        Location of a user-provided parafile to use for parallel execution
      --parallel               Whether or not to run NONMEM in parallel mode
      --parallel_timeout int   The amount of time to wait for parallel operations in NONMEM before timing out (default 2147483647)
      --prcompile              RAW NMFE OPTION - Forces PREDPP compilation
      --prdefault              RAW NMFE OPTION - Do not recompile any routines other than FSUBS
  -p, --preview                preview action, but don't actually run command
      --prsame                 RAW NMFE OPTION - Indicates to NONMEM that the PREDPP compilation step should be skipped
      --threads int            number of threads to execute with locally or nodes to execute on in parallel (default 4)
      --tprdefault             RAW NMFE OPTION - Test if is okay to do -prdefault
  -v, --verbose                verbose output
```

### SEE ALSO

* [bbi nonmem](bbi_nonmem.md)	 - nonmem a (set of) models locally or on the grid

