## bbi nonmem

Run (and other actions) against nonmem model(s)

### Synopsis

Nonmem and its subcommands are all based around the execution and interpretation of nonmem models and more. 

### Options

```
  -h, --help               help for nonmem
      --nm_version string   Version of nonmem from the configuration list to use
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

### Subcommands
* [clean](clean/clean.md)
* [probs](probs/probs.md)
* [reclean](reclean/reclean.md)
* [run](run/run.md)
* [scaffold](scaffold/scaffold.md)
* [summary](summary/summary.md)


### nmVersion
The nm_version flag is a string representing a key in the bbi.yml loaded from the model directory. Given the below sample:

```yml
nm_version: nm74gf
overwrite: false
clean_lvl: 1
copy_lvl: 0
git: true
bbi_binary: /data/apps/bbi
save_config: false
output_dir: '{{ .Name }}'
threads: 4
debug: false
local:
  create_child_dirs: false
nonmem:
  nm73gf:
    home: /opt/NONMEM/nm73gf
    executable: nmfe73
    nmqual: true
    default: false
  nm73gf_nmfe:
    home: /opt/NONMEM/nm73gf_nmfe
    executable: nmfe73
    nmqual: false
    default: false
  nm74gf:
    home: /opt/NONMEM/nm74gf
    executable: nmfe74
    nmqual: true
    default: false
  nm74gf_nmfe:
    home: /opt/NONMEM/nm74gf_nmfe
    executable: nmfe74
    nmqual: false
    default: false
parallel: false
delay: 0
nmqual: false
json: false
log_file: ""
nmfe_options:
  license_file: ""
  prsame: false
  background: false
  prcompile: false
  prdefault: false
  tprdefault: false
  nobuild: false
  maxlim: 100
mpi_exec_path: /usr/local/mpich3/bin/mpiexec
parallel_timeout: 2147483647
parafile: ""
```

Using an `nmVersion` of nm74_gf would load the nonmem binary named `nmfe74` from `ls/opt/NONMEM/nm74gf/run` during nonmem local execution