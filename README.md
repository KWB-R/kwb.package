[![R-CMD-check](https://github.com/KWB-R/kwb.package/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.package/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.package/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.package/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.package/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.package)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.package)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.package)](https://kwb-r.r-universe.dev/)

# kwb.package

This package contains some helper functions for (un-)installing KWB packages and 
for showing package dependencies. The function of main interest may be
`updateKwbPackages()` that checks for the latest package version on
KWB's server and installs the packages from there if required.


## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.package' from GitHub
remotes::install_github("KWB-R/kwb.package")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.package](https://kwb-r.github.io/kwb.package)

Development: [https://kwb-r.github.io/kwb.package/dev](https://kwb-r.github.io/kwb.package/dev)
