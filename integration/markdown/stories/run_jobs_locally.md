## Run NonMem jobs locally
**Product Risk**: High

### Summary
As a user, I would like to be able to execute NonMem jobs locally, without needing the grid, if NonMem is installed on 
the system. 

#### Tests

##### Automated Tests

Test | Test Name | Count
-----|-----------|-------
bbi_local_test.go| TestBbiCompletesLocalExecution | 1
bbi_local_test.go| TestBbiParallelExecution | 1
