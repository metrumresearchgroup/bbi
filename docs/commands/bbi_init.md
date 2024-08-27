## bbi init

Create configuration file with defaults

### Synopsis

Write a bbi.yaml configuration file in the current directory
to initialize it for running bbi.

If the --dir=DIR option is specified, DIR should point to a directory that
contains one or more NONMEM installations directories. Any subdirectory in
DIR is taken as an installation if it has the expected layout (e.g., a "run"
directory with an nmfe executable)

```
bbi init [flags]
```

### Options

```
      --dir strings   directory in which to look for NONMEM installations
  -h, --help          help for init
```

### Options inherited from parent commands

```
  -d, --debug           debug mode
      --json            show JSON output, if possible
  -o, --output string   output file
  -p, --preview         preview action, but don't actually run command
      --threads int     number of threads to execute with locally or nodes to execute on in parallel (default 4)
  -v, --verbose         verbose output
```

### SEE ALSO

* [bbi](bbi.md)	 - Manage and execute models

