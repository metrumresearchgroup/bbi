## bbi nonmem run sge

Targets a model or collection of models for execution on the Sun Grid Engine

### Synopsis

Specifies to run the targeted model on the Sun Grid Engine. The SGE execution method basically operates as a qsub wrapper for `bbi nonmem run local` to ensure uniformity.

### bbi Binary Location
You may note that this command has an additional flag, the `--bbi_binary` flag. This is because the SGE command will create a script that will basically run bbi in local mode as a SGE command. This means that the job being fired off onto the grid isn't saying

`"nmfe74 ..."`

It's saying:

`bbi run nonmem local ...`

This means that:

* All of the hygenics of BBI execution are being completed asynchronously
* The only difference in execution for local vs SGE all follow local execution in the end
* There is no difference in output / structure  between local or SGE execution

In order for this to work, when building the script to send to SGE, we will need to know where bbi's binary is
located. Thankfully this is handled automatically by bbi. When you initialize BBI, it'll automatically set the 
`bbi_binary` value based on the location BBI was run from. No need to manually set this. 

### Options

```
    --bbi_binary string   directory path for bbi to be called in goroutines (SGE Execution) (default "/data/apps/bbi")
```

### Sample Output
```
2019/12/23 18:22:07 expanding model pattern:[001:009].mod
2019/12/23 18:22:07 Configuration file successfully loaded from /240/bbi.yaml
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
