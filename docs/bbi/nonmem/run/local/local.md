## bbi nonmem run local

Targets a model or collection of models for execution on the local machine

### Synopsis

Specifies to run the targeted model on the local machine

### Options

```
--create_child_dirs   Indicates whether or not local branch executionshould create a new subdirectory with the output_dir variable as its name and execute in that directory (defaulttrue)
```

The `create_child_dirs` flag is used to determine whether a new directory should be created for nonmem to place its
output into. By default this is no, but whenever a job is run in the SGE mode, the SGE phase will create a directory
so the local mode of execution will be set to no such that it does not create another child directory underneath. 

This also avoids issues dealing with overwrite. This pattern allows us to maintain resiliency of files across execution 
modes and not have to have constant override logic for `overwrite`

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