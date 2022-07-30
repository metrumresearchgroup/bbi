## bbi nonmem run sge

Targets a model or collection of models for execution on the Sun Grid Engine

### Synopsis

Specifies to run the targeted model on the Sun Grid Engine. The SGE execution method basically operates as a qsub wrapper for `bbi nonmem run local` to ensure uniformity.

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
