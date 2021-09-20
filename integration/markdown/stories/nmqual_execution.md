## NonMem Execution via NMQual
**Product Risk**: low

### Summary
As a user, I would like to have the option to execute NonMem the same way that autolog.pl did so that NMQual can be
used. I would like an option exposed that will trigger bbi to specify autolog.pl syntax in its executable script, 
rather than the typical calls directly to NMFE.

#### Tests

##### Automated Tests

Test | Test Name | Count
-----|-----------|-------
nmqual_test.go| TestNMQUALExecutionSucceeds |1 
