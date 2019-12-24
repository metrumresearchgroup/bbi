## bbi nonmem

Run (and other actions) against nonmem model(s)

### Synopsis

Nonmem and its subcommands are all based around the execution and interpretation of nonmem models and more. 

### Options

```
  -h, --help                  help for nonmem
      --nmExecutable string   Name of nonmem executable to use. Defaults to nmfe74 (NM7.4) (default "nmfe74")
      --config string   config file (default is $HOME/babylon.yaml)
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

### Subcommands
* [clean](clean/clean.md)
* [probs](probs/probs.md)
* [reclean](reclean/reclean.md)
* [run](run/run.md)
* [scaffold](scaffold/scaffold.md)
* [summary](summary/summary.md)
