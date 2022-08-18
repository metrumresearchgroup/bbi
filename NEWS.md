
# bbi 3.1.1

* Teach `bbi nonmem summary` how to handle `ONLYSIM` runs.  (#224)


# bbi 3.1.0

Primarily, this release contains a refactored test suite which fixes a
number of broken unit tests and incorporates integration tests
formerly in the (now archived) [bbitest repo](https://github.com/metrumresearchgroup/bbitest)
into [the core bbi repo](https://github.com/metrumresearchgroup/bbi/tree/develop/integration).

Additionally, beginning with this release, only 64-bit binaries will
be built by `goreleaser`.

## New features

* `bbi nonmem params` has been added. This takes a `--dir` argument
  and will output a table with final parameter estimates for all
  models in that directory. The output is in `.csv` format, and can be
  parsed to a tibble by `bbr::param_estimates_batch()`.


# bbi 3.0.3

* Fixed a bug where `condition_number` would come back as `1.0` in
  situations where it shouldn't. (#214)

* Added `eigenvalue_issues` heuristic, triggered when any of the of
  the eigenvalues <= 0, or if NONMEM needs to force them to be
  positive. (#216)


# bbi 3.0.2

The only change in this release is that the `bbi nonmem summary`
output will now refer to individuals in the data set as `"Subjects"`
instead of `"Patients"`, in accordance with the terminology widely
used in scientific and medical literature.


# bbi 3.0.1

Primarily a patch to fix a bug when parsing output from NONMEM 7.5
runs (#205). Also ran `gofmt` and cleaned up some old files and
documentation that had gotten stale and was no longer in use or
accurate (#206).


# bbi 3.0.0

## Rename to bbi

There are no functional changes in this release, but it represents the
official rename from `babylon` to `bbi`. Previously `bbi` was used as
an alias for the CLI to `babylon` and by default the built binary was
saved as `bbi`. Going forward it is the name of the entire app. The
[accompanying R package](https://github.com/metrumresearchgroup/bbr)
will now be known as `bbr`.


# babylon 2.3.1

## Bug fixes

* Increased buffer size for reading lines from the `.ext` file to
  256k. (#191)

* Changed checks for Bayesian and Non-gradient estimation methods to
  partial string matching so that more estimation methods are
  correctly matched. (#191)

* Changed lower-diagonal lookup to a dynamic function instead of a
  lookup table. This effectively means there is no longer a limit on
  the number of `ETA`'s that can be parsed from a model. (#189)

* Fixed bug where model files with a period in the name
  (i.e. `1000.1.ctl`) were being parsed incorrectly. (#192)


# babylon 2.3.0

All user-facing changes for this release only effect the `bbi nonmem
summary` command and its output. There are also some changes to the CI
configuration to facilitate rendering updated validation documents
with [babylontest](https://github.com/metrumresearchgroup/babylontest)
and [goProjectValidator](https://github.com/metrumresearchgroup/goProjectValidator).

## Additions and changes

* `bbi nonmem summary` will no longer parse the `.cov` and `.cor`
  files
  * `summary --json` output will no longer have `covariance_theta` or
    `correlation_theta` elements
  * `--no-cov-file` and `--no-cor-file` flags are deprecated

* `bbi nonmem covcor` command now returns the json output that was
  formerly contained in the `covariance_theta` or `correlation_theta`
  elements. The `--json` flag is not needed for this command. It
  always returns json.

* The following fields were added or modified in the `bbi nonmem
  summary --json` output:
  * **ofv** -- Previously included the objective function value for
    only the last estimation method. Now includes all estimation
    methods.
  * **condition_number** -- The condition number. This will only be in
    the .lst file in certain cases (when scientists ask for it in the
    $COV block `PRINT=E`). There will be one for each estimation
    method. If any of them are large, the `large_condition_number`
    boolean under `run_heuristics` will be `true`.
  * **eta_pval_significant** -- Added to `run_heuristics`. `true` if
    any `shrinkage_details.pval` < 0.05. Will be `false` if no
    shrinkage file is present.
  * **PRDERR** -- Added to `run_heuristics`. Indicates whether a
    `PRDERR` file is present in the output directory.

## Bug fixes

* Previously, the `.shk` file was not added to the `files_used`
  section of the `bbi nonmem summary --json` output, even when it was
  used. This has been fixed.


# babylon 2.1.3

Do not use for processing - there is an extraneous log statement that
prints out when asking for --json which will print an extra info log
making it unsuitable for piping into subsequent tools that expect a
clean json output. This is fixed in 2.1.4


# babylon 2.1.1

## Post Execution Hooks and CI Build

This release adds a post-execution hook to Babylon. The
`--post_work_executable` flag is used to tell Babylon the binary to
_any_ script or binary you wish to run after execution. Before
executing it, Babylon will set a series of environment variables:

* `BABYLON_MODEL_PATH`: Fully qualified path to the directory in which
  the original model was located
* `BABYLON_MODEL`: Full name (file + ext) of model. file.mod
* `BABYLON_MODEL_FILENAME`: First part of the filename. IE if the model
  is file.mod, this would be file
* `BABYLON_MODEL_EXT`: Extension of the filename. IE if model is
  file.mod, this will be mod
* `BABYLON_OUTPUT_DIR`: The directory into which the model was placed
  and ran
* `BABYLON_SUCCESSFUL`: Whether or not an error occurred. TRUE if
  completed without error. Error if errors were encountered.
* `BABYLON_ERROR`: If an error occurred, any error generated (Default
  is an empty string)

During the execution process, this is all rendered down into a
`post_processing.sh` script in the `OutputDir`. This allows for easy
troubleshooting.

What if the script you're executing actually needs more
environmentally than the above? The `--additional_post_work_envs` flag
handles that. Here you can specify in comma-separated ENV style
additional values to set into the environment:

```
--additional_post_work_envs "THIS=THAT,THESE=THOSE,BABYLON_ADDITIONAL=1"
```

The above would set the following additional environmental variables
before executing your requested script /binary:

* `THIS=THAT`
* `THESE=THOSE`
* `BABYLON_ADDITIONAL=1`

One thing you may notice after doing this is that only
`BABYLON_ADDITIONAL` shows up in the `post_processing.sh` script. This
is on purpose. We only write environment values that are prefixed with
`BABYLON` to the `post_processing.sh` script. Using this pattern, you
can feel free to add additional environment variables for security
(Such as auth tokens for Slack) without worrying about them being
written to the file, or add any number of other variables that get
added to the file for reproducibility.


# babylon 0.3.0

This release represents an expansion of the capabilities of the bbq
server to handle both ping/version endpoints to introspect information
about a running server.

This also represents an expansion in the information tracked for a
model. Error handling has been made slightly more robust, with errors
bubbling up to store in the database with an ERROR status for failed
run.

For execution, the ability to specify a custom directory has been
implimented through the --runDir flag via the CLI or through the
ProposedRunDir run setting through the server structure.


# babylon 0.2.0

This version adds a --oneEst flag to the cli when doing `bbi
run`. Given a model has already been run, and an estimation directory
is present, the model will be skipped. This can be quite useful when
running a directory of models, then subsequently adding/removing some
models, while leaving the estimation records for others. When trying
to re-run the models, the entire directory can be run, and --oneEst
will skip all models that already have been estimated.
