## Notify about issues with the data referenced in the control stream
**Product Risk**: low

### Summary
As a user, I would like to be notified, and have model execution stop, if I target a NonMem control stream with bbi, 
but the data file referenced therein cannot be located. 

#### Tests

##### Automated Tests

Test | Test Name | Count
-----|-----------|-------
data_test.go| TestHasValidPathForCTL | 1
data_test.go| TestHasInvalidDataPath | 1
data_test.go| TestHasValidComplexPathCTLAndMod | 1
