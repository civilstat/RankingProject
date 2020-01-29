## Test environments
* macOS 10.13.6 (on travis-ci), R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.2
* local Ubuntu 14.04 install, R 3.4.4
* local Windows 10 install, R 3.6.2
* win-builder (devel and release)
* rhub::check_for_cran()

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Vignette build requirements
Per your policies [1], this package's `tikz`-based vignettes are enabled only
on the maintainer’s machine because they take a long time (5-15 minutes) to run.
However, the HTML vignette `intro`, which is enabled everywhere, does exercise
all the features of the package.

[1] https://cran.r-project.org/web/packages/policies.html

    If the package needs special treatment (for example if vignettes can only be
    run or re-built on the maintainer’s machine or take a very long time),
    say so on the submission form.

    Long-running tests and vignette code can be made optional for checking, but
    do ensure that the checks that are left do exercise all the features of the
    package.
