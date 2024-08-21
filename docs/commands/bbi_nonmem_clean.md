## bbi nonmem clean

Clean files and folders

### Synopsis

Clean the files and directories that match the specified patterns.
Whether the pattern is interpreted as a glob or regex is controlled by the
--regex flag.

If files were copied to the parent directory automatically after model
execution (via the copy_lvl configuration), the original files in the run
directory can be cleaned up by selecting the run with the --copiedRuns
option.

```
bbi nonmem clean [flags] <pattern> [<pattern>...]
```

### Examples

```
  # Remove items in the current directory that end with ".mod"
  bbi nonmem clean *.mod
  # The same as above but ensure only files are removed
  bbi nonmem clean --filesOnly *.mod

  # Remove files in the current directory that start with "run" and end with
  # ".mod" or ".lst"
  bbi nonmem clean --filesOnly --regex "run.*\.(mod|lst)$"
  # Report what the above would remove but don't actually do it
  bbi nonmem clean --preview --filesOnly --regex "run.*\.(mod|lst)$"

  # Remove copied files (recorded in '{run}_copied.json' by 'bbi run') for
  # run001, run002, run003, and run100
  bbi nonmem clean --copiedRuns='run[001:003],run100'
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
      --background             RAW NMFE OPTION - tell NONMEM not to scan stdin for control characters
  -d, --debug                  debug mode
      --json                   show JSON output, if possible
      --licfile string         RAW NMFE OPTION - NONMEM license file to use
      --maxlim int             RAW NMFE OPTION - set the maximum values for the buffers used by NONMEM (if 0, don't pass -maxlim to nmfe) (default 2)
      --mpi_exec_path string   fully qualified path to mpiexec to use for NONMEM parallel operations (default "/usr/local/mpich3/bin/mpiexec")
      --nm_version string      version of NONMEM from the configuration list to use
      --nmqual                 whether to execute with nmqual (autolog.pl)
      --nobuild                RAW NMFE OPTION - do not build a new NONMEM executable
  -o, --output string          output file
      --parafile string        location of a user-provided parafile to use for parallel execution
      --parallel               whether to run NONMEM in parallel mode
      --parallel_timeout int   amount of time to wait for parallel operations in NONMEM before timing out (default 2147483647)
      --prcompile              RAW NMFE OPTION - forces PREDPP compilation
      --prdefault              RAW NMFE OPTION - do not recompile any routines other than FSUBS
  -p, --preview                preview action, but don't actually run command
      --prsame                 RAW NMFE OPTION - tell NONMEM to skip the PREDPP compilation step
      --threads int            number of threads to execute with locally or nodes to execute on in parallel (default 4)
      --tprdefault             RAW NMFE OPTION - test if is okay to do -prdefault
  -v, --verbose                verbose output
```

### SEE ALSO

* [bbi nonmem](bbi_nonmem.md)	 - Entry point for NONMEM-related subcommands

