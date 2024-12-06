## bbi nonmem

Entry point for NONMEM-related subcommands

```
bbi nonmem [flags]
```

### Examples

```
  # Execute model run001
  bbi nonmem run (local|sge|slurm) run001.mod
  #  Run models run001.mod, run002.mod, and run003.mod
  bbi nonmem run (local|sge|slurm) 'run[001:003].mod'
  # Run all models in the current directory
  bbi nonmem run (local|sge|slurm) .

  # Summarize run001
  bbi nonmem summary run001/run001.lst
  # The extension may be omitted
  bbi nonmem summary run001/run001
  # Output JSON summary for run001 and run002
  bbi nonmem summary --json run001/run001 run002/run002

  # Display .cov/cor values from run001/run001.{cov,cor}
  bbi nonmem covcor run001/run001
  # Display the same values by specifying a full output file
  bbi nonmem covcor run001/run001.cov

```

### Options

```
      --background             RAW NMFE OPTION - tell NONMEM not to scan stdin for control characters
  -h, --help                   help for nonmem
      --licfile string         RAW NMFE OPTION - NONMEM license file to use
      --maxlim int             RAW NMFE OPTION - set the maximum values for the buffers used by NONMEM (if 0, don't pass -maxlim to nmfe) (default 2)
      --mpi_exec_path string   fully qualified path to mpiexec to use for NONMEM parallel operations (default "/usr/local/mpich3/bin/mpiexec")
      --nm_version string      version of NONMEM from the configuration list to use
      --nmqual                 whether to execute with nmqual (autolog.pl)
      --nobuild                RAW NMFE OPTION - do not build a new NONMEM executable
      --parafile string        location of a user-provided parafile to use for parallel execution
      --parallel               whether to run NONMEM in parallel mode
      --parallel_timeout int   amount of time to wait for parallel operations in NONMEM before timing out (default 2147483647)
      --prcompile              RAW NMFE OPTION - forces PREDPP compilation
      --prdefault              RAW NMFE OPTION - do not recompile any routines other than FSUBS
      --prsame                 RAW NMFE OPTION - tell NONMEM to skip the PREDPP compilation step
      --tprdefault             RAW NMFE OPTION - test if is okay to do -prdefault
```

### Options inherited from parent commands

```
  -d, --debug           debug mode
      --json            show JSON output, if possible
  -o, --output string   output file
  -p, --preview         preview action, but don't actually run command
      --threads int     number of threads to execute with locally or nodes to execute on in parallel (default 4)
  -v, --verbose         verbose output
```

### SEE ALSO

* [bbi](bbi.md)	 - Manage and execute models
* [bbi nonmem clean](bbi_nonmem_clean.md)	 - Clean files and folders
* [bbi nonmem covcor](bbi_nonmem_covcor.md)	 - Display .cov and .cor output for a model
* [bbi nonmem params](bbi_nonmem_params.md)	 - Extract the parameter estimates of models
* [bbi nonmem probs](bbi_nonmem_probs.md)	 - Summarize model definitions in a directory
* [bbi nonmem reclean](bbi_nonmem_reclean.md)	 - Clean files in run directory by specified level
* [bbi nonmem run](bbi_nonmem_run.md)	 - Run models locally or on the grid
* [bbi nonmem scaffold](bbi_nonmem_scaffold.md)	 - Scaffold directory structures
* [bbi nonmem summary](bbi_nonmem_summary.md)	 - Summarize model results

