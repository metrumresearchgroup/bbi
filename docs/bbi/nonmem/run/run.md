## bbi nonmem run

Targets a model or collection of models for execution

### Synopsis

The run command collects a series of flags necessary to define the behavior for:

* How to run a nonmem model
    * Locally
    * Sun Grid Engine
* How (if at all) to sanitize the output directory
* How (if at all) to copy result files
* How (if at all) to handle gitignore files

#### Important Flags and Definitions

* `--overwrite=true` : Specifies that, if an output directory for a given model exists, to remove all of it's contents and re-run the model. Default is true
* `--git=true` : Specifies the following:
     * During initial model execution, a wildcard (`*`) gitignore file is placed into the model execution directory
        * While not explicitly necessary, if you are using something like RStudio or other platform that is tracking changes to git, this will keep those other platforms sane while nonmem is generating all of its temporary files. Vastly important when multiple runs are being done at once. 
     * After execution is done, the gitignore file is updated with various temp files to prevent them from being committed into a repo on accident
* `--output_dir {{ .Name }}` : A valid go-template (Defaults to the one listed here) that will be used for creating the directories in which model execution will occur. `Name` is the only variable interpreted, but you can append / prepend items to it:
    * `'{{ .Name }}_output` : yields modelname_output
    * `'output_{{ .Name }}'` : yields output_modelname
* `--clean_lvl <1|2|3>` : Based on a list of extensions and files (See below), will remove any matching files from the output directory after the work is done. Default is 2
* `--copy_lvl <1|2|3>` : Based on a list of extension and files (See below), will remove copy any of the matched files back into the original model directory prepended with the model name. Mirrors PSN functionality, although the default is 0 (or off)

### Options

```
      --clean_lvl int       clean level used for file output from a given (set of) runs (default 1)
      --config string       Path (relative or absolute) to another bbi.yaml to load
      --copy_lvl int        copy level used for file output from a given (set of) runs
      --delay int           Selects a random number of seconds between 1 and this value to stagger / jitter job execution. Assists in dealing with large volumes of work dealing with the same data set. May avoid NMTRAN issues about not being able read / close files
      --git                 whether git is used
  -h, --help                help for run
      --log_file string     If populated, specifies the file into which to store the output / logging details from bbi
      --output_dir string   Go template for the output directory to use for storging details of each executed model (default "{{ .Name }}")
      --overwrite           Whether or not to remove existing output directories if they are present
      --save_config         Whether or not to save the existing configuration to a file with the model (default true)
```

### Subcommands
* [local](local/local.md) - Nonmem model execution
* [sge](sge/sge.md) - check version


### Turnstile Execution Control
The run command and its variants all implement the [turnstile](https://github.com/metrumresearchgroup/turnstile) workflow to manage concurrency of model execution. At the top level, bbi takes a `--threads` option. Whatever this value is set to is the maximum amount of ongoing work turnstile will allow. For local and grid execution this allows you to controllably stagger the work being doled out. 

### Clean Level Files
Currently there is really only one level of clean files, which is level 1. Its contents are the temporary files (including the nonmem executable) created during nonmem execution:

```
    "background.set",
	"compile.lnk",
	"FCON",
	"FDATA",
	"FMSG",
	"FREPORT",
	"FSIZES",
	"FSTREAM",
	"FSUBS",
	"FSUBS.0",
	"FSUBS.o",
	"FSUBS_MU.F90",
	"FSUBS.f90",
	"fsubs.f90",
	"FSUBS2",
	"gfortran.txt",
	"GFCOMPILE.BAT",
	"INTER",
	"licfile.set",
	"linkc.lnk",
	"LINK.LNK",
	"LINKC.LNK",
	"locfile.set",
	"maxlim.set",
	"newline",
	"nmexec.set",
	"nmpathlist.txt",
	"nmprd4p.mod",
	"nobuild.set",
	"parafile.set",
	"parafprint.set",
	"prcompile.set",
	"prdefault.set",
	"prsame.set",
	"PRSIZES.f90",
	"rundir.set",
	"runpdir.set",
	"simparon.set",
	"temp_dir",
	"tprdefault.set",
	"trskip.set",
	"worker.set",
	"xmloff.set",
	"fort.2001",
	"fort.2002",
	"flushtime.set",
	"nonmem"
```

### Copy Level Files

Copy-up funtionality is broken out into three tiers

#### Tier 1
Tier 1 consists of:

* Any files named in the nonmem model table definitions
* Any files beginning with the model filename and ending with the following extensions:

```
    	".xml",
		".grd",
		".shk",
		".cor",
		".cov",
		".ext",
		".lst"
```

#### Tier 2
Any files beginning with the model filename and ending with the following extensions:

```
        ".clt",
		".coi",
		".clt",
		".coi",
		".cpu",
		".shm",
		".phi",
```


#### Tier 3
Any files beginning with the model filename and ending with the followin extensions:

```
		"_ETAS",
		"_RMAT",
		"_SMAT",
		".msf",
		"_ETAS.msf",
		"_RMAT.msf",
		"_SMAT.msf",
```