## bbi nonmem run local

Targets a model or collection of models for execution on the local machine

### Synopsis

Specifies to run the targeted model on the local machine

### Options

```
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
$ ./bbi nonmem run local 240/[001:009].mod
2019/12/23 18:12:46 expanding model pattern: 240/[001:009].mod
2019/12/23 18:12:46 Configuration file successfully loaded from 240/babylon.yml
2019/12/23 18:12:46 A total of 9 models have been located for work
2019/12/23 18:12:46 9 models successfully completed initial setup phase.
2019/12/23 18:12:46 Beginning local work phase for 001
2019/12/23 18:12:46 Beginning local work phase for 002
2019/12/23 18:12:53 Beginning cleanup phase for model 002
2019/12/23 18:12:53 Beginning local work phase for 003
2019/12/23 18:12:53 Beginning cleanup phase for model 001
2019/12/23 18:12:53 Beginning local work phase for 004
2019/12/23 18:13:01 Beginning cleanup phase for model 003
2019/12/23 18:13:01 Beginning local work phase for 005
2019/12/23 18:13:02 Beginning cleanup phase for model 004
2019/12/23 18:13:02 Beginning local work phase for 006
2019/12/23 18:13:09 Beginning cleanup phase for model 005
2019/12/23 18:13:09 Beginning local work phase for 007
2019/12/23 18:13:09 Beginning cleanup phase for model 006
2019/12/23 18:13:09 Beginning local work phase for 008
2019/12/23 18:13:17 Beginning cleanup phase for model 008
2019/12/23 18:13:17 Beginning local work phase for 009
2019/12/23 18:13:17 Beginning cleanup phase for model 007
2019/12/23 18:13:23 Beginning cleanup phase for model 009
9 models completed in 37.964110535s
```