## Update to maintainer's address
I am still the same maintainer (Jerzy Wieczorek), but I am changing the email address listed in DESCRIPTION from my home address (jerzywieczorek@gmail.com) to my work address (jawieczo@colby.edu) instead.

## Test environments
* local Windows 10 install, R 4.1.2
* devtools::check_rhub()
* devtools::check_win_release()
* devtools::check_win_devel()
* GitHub Actions for macOS-latest (release), windows-latest (release), ubuntu-latest (devel), ubuntu-latest (release), ubuntu-latest (oldrel-1)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Vignette build requirements
Per CRAN policies [1], this package's `tikz`-based vignettes are precompiled on the maintainer’s machine because they take a long time (about 15 minutes) to run. The precompiled PDFs are included as vignettes by using `R.rsp`.
However, the HTML vignette `intro`, which is enabled everywhere, does exercise all the features of the package.

[1] https://cran.r-project.org/web/packages/policies.html

    If the package needs special treatment (for example if vignettes can only be run or re-built on the maintainer’s machine or take a very long time), say so on the submission form.

    Long-running tests and vignette code can be made optional for checking, but do ensure that the checks that are left do exercise all the features of the package.
