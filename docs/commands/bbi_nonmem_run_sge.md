## bbi nonmem run sge

Run models on the Sun Grid Engine

```
bbi nonmem run sge [flags] <model> [<model>...]
```

### Examples

```
  # Execute model run001
  bbi nonmem run sge run001.mod
  #  Run models run001.mod, run002.mod, and run003.mod
  bbi nonmem run sge 'run[001:003].mod'
  # Run all models in the current directory
  bbi nonmem run sge .
```

### Options

```
      --bbi_binary string         path to bbi executable to be called in goroutines (SGE Execution)
      --grid_name_prefix string   Any prefix you wish to add to the name of jobs being submitted to the grid
  -h, --help                      help for sge
```

### Options inherited from parent commands

```
      --additional_post_work_envs strings   additional values (as ENV KEY=VALUE) to provide for the post execution environment
      --background                          RAW NMFE OPTION - tell NONMEM not to scan stdin for control characters
      --clean_lvl int                       clean level used for output (default 1)
      --config string                       path to another bbi.yaml to load
      --copy_lvl int                        copy level used for output
  -d, --debug                               debug mode
      --delay int                           Selects a random number of seconds between 1 and this value to stagger / jitter job execution. Assists in dealing with large volumes of work dealing with the same data set. May avoid NMTRAN issues about not being able read / close files
      --git                                 whether git is used
      --json                                show JSON output, if possible
      --licfile string                      RAW NMFE OPTION - NONMEM license file to use
      --log_file string                     file into which to store the output / logging details from bbi
      --maxlim int                          RAW NMFE OPTION - set the maximum values for the buffers used by NONMEM (if 0, don't pass -maxlim to nmfe) (default 2)
      --mpi_exec_path string                fully qualified path to mpiexec to use for NONMEM parallel operations (default "/usr/local/mpich3/bin/mpiexec")
      --nm_version string                   version of NONMEM from the configuration list to use
      --nmqual                              whether to execute with nmqual (autolog.pl)
      --nobuild                             RAW NMFE OPTION - do not build a new NONMEM executable
  -o, --output string                       output file
      --output_dir string                   Go template for the output directory to use for storing details of each executed model (default "{{ .Name }}")
      --overwrite                           whether to remove existing output directories
      --parafile string                     location of a user-provided parafile to use for parallel execution
      --parallel                            whether to run NONMEM in parallel mode
      --parallel_timeout int                amount of time to wait for parallel operations in NONMEM before timing out (default 2147483647)
      --post_work_executable string         script or binary to run when job execution completes or fails
      --prcompile                           RAW NMFE OPTION - forces PREDPP compilation
      --prdefault                           RAW NMFE OPTION - do not recompile any routines other than FSUBS
  -p, --preview                             preview action, but don't actually run command
      --prsame                              RAW NMFE OPTION - tell NONMEM to skip the PREDPP compilation step
      --save_config                         whether to save the existing configuration to the output directory (default true)
      --threads int                         number of threads to execute with locally or nodes to execute on in parallel (default 4)
      --tprdefault                          RAW NMFE OPTION - test if is okay to do -prdefault
  -v, --verbose                             verbose output
```

### SEE ALSO

* [bbi nonmem run](bbi_nonmem_run.md)	 - Run models locally or on the grid

