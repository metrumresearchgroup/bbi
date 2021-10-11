# bbitest
This directory contains integration tests data. Many of these tests must be run in an environment containing a working NONMEM installation and/or worker nodes that can be used to execute jobs. See [the `.drone.yml` in `bbi`](https://github.com/metrumresearchgroup/bbi/blob/develop/.drone.yml) for examples.

## Refreshing golden files
The `bbi nonmem summary` tests (in `integration/bbi_summary_test.go`) use golden files stored in `integration/testdata/bbi_summary/aa_golden_files/`. These need to be refreshed when the relevant functionality in `bbi` changes. This can be done by running the tests with the `UPDATE_SUMMARY=true` environment variable, like so:
```
UPDATE_SUMMARY=true ROOT_EXECUTION_DIR=/tmp/ go test -v -run TestSum
```

Several other tests use a similar pattern:

* Tests in `integration/bbi_covcor_test.go` use `UPDATE_COVCOR`
* Tests in `integration/bbi_params_test.go` use `UPDATE_PARAMS`
