# RUN THIS MANUALLY
# to precompile and compress PDFs for the two PDF vignettes
# whenever I update the code / R package
# and before I resubmit to CRAN.

# First, I tried to use this approach:
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/
# Keep a record of the actual original Rmd source
# in "[vignettename].Rmd.orig" files,
# but precompile the lengthy tikzDictionary computations
# (and save the intermediate tex files needed for images)
# so that the resulting "[vignettename].Rmd" files are very quick to knit
# and R *thinks* it has knitted the vignettes
# so it builds the vignette metadata correctly
# and they show up when you ask for a list of the package's vignettes
# More examples:
# https://github.com/ropensci/eia/blob/master/vignettes/precompile.R
# https://rdrr.io/cran/rccmisc/src/vignettes/precompile.R
# https://rdrr.io/cran/incadata/src/vignettes/precompile.R

# HOWEVER, I could not seem to get the package to find all vignettes correctly,
# and CRAN check NOTEs still complain about needing to compress the output PDFs.
# So, we are trying the R.rsp package instead:
# https://cran.r-project.org/web/packages/R.rsp/index.html
# This way we can precompile and compress the PDFs,
# and use these pre-processed PDFs directly as the final vignette docs.
# The resulting R package doesn't seem to display source code on its own,
# but that's OK since the relevant code is displayed within each vignette,
# and meanwhile the full source is on github.

setwd("vignettes")

knitr::knit("primer.Rmd.orig", "primer.pdf")
knitr::purl("primer.Rmd.orig", "primer.R")
tools::compactPDF("primer.pdf", gs_quality = "ebook")

knitr::knit("joint.Rmd.orig", "joint.pdf")
knitr::purl("joint.Rmd.orig", "joint.R")
tools::compactPDF("joint.pdf", gs_quality = "ebook")

setwd("..")


# Note: I also tried to run
# devtools::build_vignettes()
# but this didn't seem to work right;
# it did create Meta/vignette.rds
# but it also moved files around,
# removing some of the source from vignettes folder
# and putting new things in doc folder.
# So I undid all that, and the final version I have is *not* based on this.
#
# Also, I tried the various RStudio Build tab options,
# but they didn't seem to get the vignettes right either!
#
# So instead, I just do devtools::build()
# and install from source using the resulting tarball,
# which seems to install a package with the right vignettes.

