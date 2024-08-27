## bbi nonmem summary

Summarize model results

### Synopsis

Summarize the results of the specified *.lst files. By default, this
prints a table of parameter estimates preceded by a lines with details about
the run. Pass the --json flag to get a machine-readable output that includes
more details.

The path may also be specified without the trailing ".lst". *.res files are
also supported.

```
bbi nonmem summary [flags] <lst> [<lst>...]
```

### Examples

```
  # Summarize run001
  bbi nonmem summary run001/run001.lst
  # The extension may be omitted
  bbi nonmem summary run001/run001
  # Output JSON summary for run001 and run002
  bbi nonmem summary --json run001/run001 run002/run002
```

### Options

```
      --ext-file string   name of custom ext-file
  -h, --help              help for summary
      --no-ext-file       do not use ext file
      --no-grd-file       do not use grd file
      --no-shk-file       do not use shk file
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

