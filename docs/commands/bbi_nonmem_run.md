## bbi nonmem run

run a (set of) models locally or on the grid

### Synopsis

run nonmem model(s), for example: 
bbi nonmem run <local|sge> run001.mod
bbi nonmem run  --clean_lvl=1 <local|sge> run001.mod run002.mod
bbi nonmem run <local|sge> run[001:006].mod // expand to run001.mod run002.mod ... run006.mod local
bbi nonmem run <local|sge> .// run all models in directory
 

```
bbi nonmem run [flags]
```

### Options

```
      --additional_post_work_envs strings   Any additional values (as ENV KEY=VALUE) to provide for the post execution environment
      --clean_lvl int                       clean level used for file output from a given (set of) runs (default 1)
      --config string                       Path (relative or absolute) to another bbi.yaml to load
      --copy_lvl int                        copy level used for file output from a given (set of) runs
      --delay int                           Selects a random number of seconds between 1 and this value to stagger / jitter job execution. Assists in dealing with large volumes of work dealing with the same data set. May avoid NMTRAN issues about not being able read / close files
      --git                                 whether git is used
  -h, --help                                help for run
      --log_file string                     If populated, specifies the file into which to store the output / logging details from bbi
      --output_dir string                   Go template for the output directory to use for storing details of each executed model (default "{{ .Name }}")
      --overwrite                           Whether or not to remove existing output directories if they are present
      --post_work_executable string         A script or binary to run when job execution completes or fails
      --save_config                         Whether or not to save the existing configuration to a file with the model (default true)
```

### Options inherited from parent commands

```
      --background             RAW NMFE OPTION - Tells nonmem not to scan StdIn for control characters
  -d, --debug                  debug mode
      --json                   json tree of output, if possible
      --licfile string         RAW NMFE OPTION - Specify a license file to use with NMFE (Nonmem)
      --maxlim int             RAW NMFE OPTION - Set the maximum values for the buffers used by Nonmem (if 0, don't pass -maxlim to nmfe) (default 2)
      --mpi_exec_path string   The fully qualified path to mpiexec. Used for nonmem parallel operations (default "/usr/local/mpich3/bin/mpiexec")
      --nm_version string      Version of nonmem from the configuration list to use
      --nmqual                 Whether or not to execute with nmqual (autolog.pl)
      --nobuild                RAW NMFE OPTION - Skips recompiling and rebuilding on nonmem executable
  -o, --output string          output file
      --parafile string        Location of a user-provided parafile to use for parallel execution
      --parallel               Whether or not to run nonmem in parallel mode
      --parallel_timeout int   The amount of time to wait for parallel operations in nonmem before timing out (default 2147483647)
      --prcompile              RAW NMFE OPTION - Forces PREDPP compilation
      --prdefault              RAW NMFE OPTION - Do not recompile any routines other than FSUBS
  -p, --preview                preview action, but don't actually run command
      --prsame                 RAW NMFE OPTION - Indicates to nonmem that the PREDPP compilation step should be skipped
      --threads int            number of threads to execute with locally or nodes to execute on in parallel (default 4)
      --tprdefault             RAW NMFE OPTION - Test if is okay to do -prdefault
  -v, --verbose                verbose output
```

### SEE ALSO

* [bbi nonmem](bbi_nonmem.md)	 - nonmem a (set of) models locally or on the grid
* [bbi nonmem run local](bbi_nonmem_run_local.md)	 - local specifies to run a (set of) models locally
* [bbi nonmem run sge](bbi_nonmem_run_sge.md)	 - sge specifies to run a (set of) models on the Sun Grid Engine

