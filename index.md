[![R-CMD-check](https://github.com/KWB-R/kwb.package/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.package/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.package/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.package/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.package/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.package)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.package)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.package)](https://kwb-r.r-universe.dev/)

This package contains some helper functions for (un-)installing KWB packages and 
for showing package dependencies. The function of main interest may be
`updateKwbPackages()` that checks for the latest package version on
KWB's server and installs the packages from there if required.


## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install kwb.package in R
install.packages('kwb.package')

# Browse the kwb.package manual pages
help(package = 'kwb.package')

```