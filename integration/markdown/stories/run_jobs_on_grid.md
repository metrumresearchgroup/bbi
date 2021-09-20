## Run NonMem jobs on the Grid
**Product Risk**: High

### Summary
As a user, I would like to be able to submit NonMem jobs to be run on a worker node in the SGE grid.

#### Tests

##### Automated Tests

Test | Test Name | Count
-----|-----------|-------
bbi_sge_test.go| TestBbiCompletesSGEExecution | 1
bbi_sge_test.go| TestBbiCompletesParallelSGEExecution | 1
