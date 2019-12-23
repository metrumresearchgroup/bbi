## bbi nonmem run sge

Targets a model or collection of models for execution on the Sun Grid Engine

### Synopsis

Specifies to run the targeted model on the Sun Grid Engine. The SGE execution method basically operates as a qsub wrapper for `bbi nonmem run local` to ensure uniformity.

### Babylon Binary Location
You may note that this command has an additional flag, the `--babylonBinary` flag. This is because the SGE command will create a script that will basically run babylon in local mode as a SGE command. This means that the job being fired off onto the grid isn't saying

`"nmfe74 ..."`

It's saying:

`bbi run nonmem local ...`

This means that:

* All of the hygenics of BBI execution are being completed asynchronously
* The only difference in execution for local vs SGE all follow local execution in the end
* There is no difference in output / structure  between local or SGE execution

In order for this to work, when building the script to send to SGE, we will need to know where babylon's binary is located. The default is /data/bbi, but it will depend on your implementation.

### Options

```
      --babylonBinary string   directory path for babylon to be called in goroutines (SGE Execution) (default "/data/bbi")
  -h, --help                   help for sge
      --cacheDir string       directory path for cache of nonmem executables for NM7.4+
      --cacheExe string       name of executable stored in cache
      --cleanLvl int          clean level used for file output from a given (set of) runs
      --config string         config file (default is $HOME/babylon.yaml)
      --copyLvl int           copy level used for file output from a given (set of) runs
  -d, --debug                 debug mode
      --git                   whether git is used
      --gitignoreLvl int      gitignore lvl for a given (set of) runs
      --json                  json tree of output, if possible
      --nmExecutable string   Name of nonmem executable to use. Defaults to nmfe74 (NM7.4) (default "nmfe74")
      --no-cor-file           do not use cor file
      --no-cov-file           do not use cov file
      --no-ext-file           do not use ext file
      --no-grd-file           do not use grd file
      --no-shk-file           do not use shk file
      --outputDir string      Go template for the output directory to use for storging details of each executed model (default "{{ .Name }}")
      --overwrite             Whether or not to remove existing output directories if they are present (default true)
  -p, --preview               preview action, but don't actually run command
      --saveConfig            Whether or not to save the existing configuration to a file with the model (default true)
      --saveExe string        what to name the executable when stored in cache
      --threads int           number of threads to execute with (default 4)
  -v, --verbose               verbose output
```

### Sample Output
```
2019/12/23 18:22:07 expanding model pattern:[001:009].mod
2019/12/23 18:22:07 Configuration file successfully loaded from /240/babylon.yml
2019/12/23 18:22:07 A total of 9 models have been located for work
2019/12/23 18:22:07 9 models successfully completed initial setup phase.
2019/12/23 18:22:07 Beginning SGE work phase for 001
2019/12/23 18:22:07 Beginning SGE work phase for 002
2019/12/23 18:22:07 Beginning SGE work phase for 003
2019/12/23 18:22:07 Beginning SGE work phase for 004
2019/12/23 18:22:07 Beginning SGE work phase for 005
2019/12/23 18:22:07 Beginning SGE work phase for 006
2019/12/23 18:22:07 Beginning SGE work phase for 007
2019/12/23 18:22:07 Beginning SGE work phase for 008
2019/12/23 18:22:07 Beginning SGE work phase for 009
9 models completed in 22.986631ms
```