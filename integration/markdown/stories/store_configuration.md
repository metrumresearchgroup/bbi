## Capture all configurations and write to a file
**Product Risk**: high

### Summary
As a user, I would like to have all configurations used for each run captured and rendered into files that can be 
stored in version control, for the sake of reproducibility. A file should be created in the model output folder which 
contains the merged configurations between any flags provided, configuration files and default values to indicate 
exactly how the model was executed. 

#### Tests

##### Automated Tests

Test | Test Name | Count
-----|-----------|-------
config_test.go| TestBBIConfigJSONCreated |1 
