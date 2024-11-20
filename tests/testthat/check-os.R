library(testthat)        # load testthat package

devtools::load_all()
library(CTTvis)       # load the package

context("check-os")

# The following function runs a local R CMD check
devtools::check()

# Check for CRAN specific requirements
rhub::check_for_cran()
devtools::document()
