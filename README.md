
# bbi

`bbi` is a command-line interface for running NONMEM models and
extracting information from the results.  It provides core
functionality for the [bbr][] R package, which implements many
additional features for managing and interacting with NONMEM models.

Documentation for `bbi` is available at
<https://metrumresearchgroup.github.io/bbi/docs/>.

[bbr]: https://github.com/metrumresearchgroup/bbr

### Development

To run the test suite, you can invoke [scripts/run-unit-tests][ru] and
[scripts/run-integration-tests][ri] directly or via `make vt-test`.

After updating a subcommand, regenerate the Markdown documentation at
[docs/commands][dc] by running `make vt-gen-docs`.  See `make vt-help` and
[internal/valtools/README.md][vr] for more details on the validation tooling.

The setup for building the documentation site is described in
[docs/site/README.md][dr].

<!-- Note: Use GitHub URLs rather than relative paths (e.g., "/docs/commands") so -->
<!-- that these work on https://metrumresearchgroup.github.io/bbi/ without further -->
<!-- processing. -->
[dc]: https://github.com/metrumresearchgroup/bbi/blob/main/docs/commands
[dr]: https://github.com/metrumresearchgroup/bbi/blob/main/docs/site/README.md
[ri]: https://github.com/metrumresearchgroup/bbi/blob/main/scripts/run-integration-tests
[ru]: https://github.com/metrumresearchgroup/bbi/blob/main/scripts/run-unit-tests
[vr]: https://github.com/metrumresearchgroup/bbi/blob/main/internal/valtools/README.md
