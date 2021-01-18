[![Go Report Card](https://goreportcard.com/badge/github.com/metrumresearchgroup/bbi)](https://goreportcard.com/report/github.com/metrumresearchgroup/bbi)

# bbi

bbi is (will be) a complete solution for managing projects involving modeling and simulation with a number 
of software solutions used in pharmaceutical sciences. 

Initial support encompasses NONMEM  however the api is designed in a way to be flexible to handle other software.

Most components are written in Go, a language championed by google. By using go, virtually all components are nicely cross platform,
 and can be distributed as a single binary with no (required) dependencies for the user.

### bbi CLI

command line interface for executing and managing models and projects. The nomenclature for the command line is relatively simple, following a similar pattern regardless of the modeling software or execution mode. 

`bbi <modeling software> <execution mode> path_to_model`

For Example:

`bbi nonmem run sge path/to/file.mod`

 * `nonmem` : The modeling software we should be targeting for this run
 * `sge` : The mode of execution. For nonmem this can either be local or sge, with sge indicating submission of jobs to the grid
 * `/abs/path/to/file.mod` : The location of the file to submit for execution. Can be relative or absolute. 

 #### Configuration

 While the `bbi` CLI is extremely configurable via flags on execution, such as: 

 `bbi nonmem run --cleanLvl 2 --copyLvl 1 --overwrite=true --git=true ... /path/to/file.mod`

 For automation and reproducability purposes, that can be painful, leading to scripting purely for the purpose of re-executing the job the same was it was originally run. This can be remedied via a `yaml` config file in one of two ways:

 * The `--saveConfig` flag will take all the flags you have passed and write it to `bbi.yaml` in the same directory as the model file you provide as an argument
 * You may also use the `bbi init` command in the directory with your models to create a default configuration file. you may alter this as necessary to meet your needs.

 Configurations for execution are located (in order of priority)

 * In the Model Directory
 * In the directory from which `bbi` is executed
 * In the executing user's home (`~`) directory


 #### Configuration and SGE

 You may notice that if you issue a job with nonmem targeting SGE that a `bbi.yaml` file is created for you automatically. This is because SGE execution wraps the `bbi` CLI into an executable for the grid to execute. This ensures the following:

 * Execution via SGE is done **the exact same way** that local execution for nonmem occurs. 
 * This includes cleanup, copy-up, and git operations
 * Also ensures a single execution path 

 To do this sanely, we make sure that the parameters provided on the initial SGE run are captured and stored with the model. This way, each subsequent `bbi` call made from the grid is made with the *exact same parameters*.

### Summary output

Example output of NONMEM run summary as of Jan 2021 

By controlling the CLI flags, it can either be an object that can be consumed by other programs such as R:

```

```

or human readable tabular formats such as:

```

```


### Development

Test-driven development encouraged, facilitated by goconvey to monitor coverage
and re-run tests on change(s)

During development, while installing new versions for testing, running "make install" will auto-append a timestamp to the build version.

```
$ go get github.com/smartystreets/goconvey
goconvey 
```

a web UI will be started at localhost:8080 with file watchers to rerun tests on 
file change(s)
