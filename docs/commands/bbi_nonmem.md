## bbi nonmem

nonmem a (set of) models locally or on the grid

### Synopsis


run nonmem model(s), for example: 
bbi nonmem run <local|sge> run001.mod
bbi nonmem run  --clean_lvl=1 <local|sge> run001.mod run002.mod
bbi nonmem run <local|sge> run[001:006].mod // expand to run001.mod run002.mod ... run006.mod local
bbi nonmem run <local|sge> .// run all models in directory
 

summarize model(s), for example:
bbi nonmem summary run001/run001
bbi nonmem summary run001/run001.lst
bbi nonmem summary run001/run001.res
bbi nonmem summary run001/run001 run002/run002
 

load .cov and .cor output from model(s), for example: 
bbi nonmem covcor run001/run001
bbi nonmem covcor run001/run001.cov
 


```
bbi nonmem [flags]
```

### Options

```
      --background             RAW NMFE OPTION - Tells NONMEM not to scan StdIn for control characters
  -h, --help                   help for nonmem
      --licfile string         RAW NMFE OPTION - Specify a license file to use with NMFE (NONMEM)
      --maxlim int             RAW NMFE OPTION - Set the maximum values for the buffers used by NONMEM (if 0, don't pass -maxlim to nmfe) (default 2)
      --mpi_exec_path string   The fully qualified path to mpiexec. Used for NONMEM parallel operations (default "/usr/local/mpich3/bin/mpiexec")
      --nm_version string      Version of NONMEM from the configuration list to use
      --nmqual                 Whether or not to execute with nmqual (autolog.pl)
      --nobuild                RAW NMFE OPTION - Skips recompiling and rebuilding on NONMEM executable
      --parafile string        Location of a user-provided parafile to use for parallel execution
      --parallel               Whether or not to run NONMEM in parallel mode
      --parallel_timeout int   The amount of time to wait for parallel operations in NONMEM before timing out (default 2147483647)
      --prcompile              RAW NMFE OPTION - Forces PREDPP compilation
      --prdefault              RAW NMFE OPTION - Do not recompile any routines other than FSUBS
      --prsame                 RAW NMFE OPTION - Indicates to NONMEM that the PREDPP compilation step should be skipped
      --tprdefault             RAW NMFE OPTION - Test if is okay to do -prdefault
```

### Options inherited from parent commands

```
  -d, --debug           debug mode
      --json            json tree of output, if possible
  -o, --output string   output file
  -p, --preview         preview action, but don't actually run command
      --threads int     number of threads to execute with locally or nodes to execute on in parallel (default 4)
  -v, --verbose         verbose output
```

### SEE ALSO

* [bbi](bbi.md)	 - manage and execute models
* [bbi nonmem clean](bbi_nonmem_clean.md)	 - clean files and folders
* [bbi nonmem covcor](bbi_nonmem_covcor.md)	 - load .cov and .cor output from a model run
* [bbi nonmem params](bbi_nonmem_params.md)	 - get the parameters of model(s)
* [bbi nonmem probs](bbi_nonmem_probs.md)	 - summarize information about project
* [bbi nonmem reclean](bbi_nonmem_reclean.md)	 - clean files in an estimation directory by clean level
* [bbi nonmem run](bbi_nonmem_run.md)	 - run a (set of) models locally or on the grid
* [bbi nonmem scaffold](bbi_nonmem_scaffold.md)	 - scaffold directory structures
* [bbi nonmem summary](bbi_nonmem_summary.md)	 - summarize the output of model(s)

