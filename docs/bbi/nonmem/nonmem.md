## bbi nonmem

Run (and other actions) against nonmem model(s)

### Synopsis

Nonmem and its subcommands are all based around the execution and interpretation of nonmem models and more. 

### Options

```
  -h, --help               help for nonmem
      --nmVersion string   Version of nonmem from the configuration list to use
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


### nmVersion
The nmVersion flag is a string representing a key in the babylon.yml loaded from the model directory. Given the below sample:

```yml
babylonbinary: /data/bbi
cachedir: mdlcache
cacheexe: ""
cleanlvl: 0
copylvl: 0
git: true
gitignorelvl: 1
nmexecutable: nmfe74
nmversion: nm74_gf
nobuild: false
nonmem:
  nm73_gf:
    default: false
    executable: nmfe73
    home: /opt/NONMEM/nm73gf
    nmqual: true
  nm73_nmfe:
    default: false
    executable: nmfe73
    home: /opt/NONMEM/nm73gf_nmfe
    nmqual: false
  nm74_gf:
    default: true
    executable: nmfe74
    home: /opt/NONMEM/nm74gf
    nmqual: true
  nm74_nmfe:
    default: false
    executable: nmfe74
    home: /opt/NONMEM/nm74gf_nmfe
    nmqual: false
oneest: false
outputdir: '{{ .Name }}'
overwrite: true
recleanlvl: 0
saveconfig: true
saveexe: ""
threads: 8
```

Using an `nmVersion` of nm74_gf would load the nonmem binary named `nmfe74` from `ls/opt/NONMEM/nm74gf/run` during nonmem local execution