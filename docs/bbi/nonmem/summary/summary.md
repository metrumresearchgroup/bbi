## bbi summary

summarize the output of a model

### Synopsis


run model(s), for example: 
nmu summarize run001.lst
 

```
bbi nonmem summary [flags]
```

### Options

```
  -h, --help   help for summary
```

### Options inherited from parent commands

```
  -h, --help                 help for nonmem
      --mpi_exec_path string   The fully qualified path to mpiexec. Used for nonmem parallel operations (default "/usr/local/mpich3/bin/mpiexec")
      --nm_version string     Version of nonmem from the configuration list to use
      --nodes int            The number of nodes on which to perform parallel operations (default 8)
      --parallel             Whether or not to run nonmem in parallel mode
      --timeout int          The amount of time to wait for parallel operations in nonmem before timing out (default 30)
      --config string   config file (default is bbi.yaml is directory where command is run)
  -d, --debug           debug mode
      --json            json tree of output, if possible
      --no-cor-file     do not use cor file
      --no-cov-file     do not use cov file
      --no-ext-file     do not use ext file
      --no-grd-file     do not use grd file
      --no-shk-file     do not use shk file
  -p, --preview         preview action, but don't actually run command
      --threads int     number of threads to execute with (default 4)
  -v, --verbose         verbose output
```


### SEE ALSO
* [bbi](bbi.md)	 - manage and execute models
