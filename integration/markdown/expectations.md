# Top Level Expectations
bbi is expected to be able to do the following tasks. Each is described in more detail below.

* Run NonMem jobs locally
* Run NonMem jobs on the Grid
* Notify about issues with the data referenced in the control stream
* Initialize a project with minimum configs required for execution
* Pass NMFE options directly to NonMem
* Capture all configurations and write to a file
* NonMem Execution via NMQual
* Parse model output folder
* Parse .cov and .cor files

## Glossary

* __NonMem__ : NONMEM is a software package for population pharmacokinetic modeling. Its name is an acronym for 
non-linear mixed effects modeling. It is referred to as __NONMEM__ or __NonMem__ in these docs. A __NonMem job__ refers
to a job executed by bbi that will need to invoke the NONMEM program.
* __Output Directory__ : When a model such as `001.ctl` is targeted with bbi, bbi will create a new directory
called `001`, copy the model into it, and do the execution directory. This directory called `001` is referred to
as the __Output Directory__ or __output folder__ for that model. 