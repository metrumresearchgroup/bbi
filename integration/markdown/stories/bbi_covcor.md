## Parse .cov and .cor files
**Product Risk**: low

### Summary
As a user, I want to be able to parse the `.cov` and `.cor` files in the NonMem output folder into either a machine-readable
structure like `.json`.

#### Tests

##### Automated Tests

Test | Test Name | Count
-----|-----------|-------
bbi_covcor_test.go| TestCovCorHappyPath |1
bbi_covcor_test.go| TestCovCorErrors |1
