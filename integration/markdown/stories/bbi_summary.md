## Parse model output folder
**Product Risk**: medium

### Summary
As a user, I want to be able to parse a summary of the files in the NonMem output folder into either a human-readable table, or a machine-readable
structure like `.json`.

#### Tests

##### Automated Tests

Test | Test Name | Count
-----|-----------|-------
bbi_summary_test.go| TestSummaryHappyPath |1
bbi_summary_test.go| TestSummaryArgs |1
bbi_summary_test.go| TestSummaryErrors |1
bbi_summary_test.go| TestSummaryHappyPathNoExtension |1 
