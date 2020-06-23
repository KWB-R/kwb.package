[![R build status](https://github.com/KWB-R/kwb.package/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.package/actions)
[![codecov](https://codecov.io/github/KWB-R/kwb.package/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.package)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.package)]()

# kwb.package

DEVELOPMENT VERSION! This package contains some
helper functions for (un-)installing KWB packages and for showing
package dependencies. The function of main interest may be
updateKwbPackages() that checks for the latest package version on
KWB's MEDUSA server and installs the packages from there if required.

## Installation

```r
#install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("KWB-R/kwb.package")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.package](https://kwb-r.github.io/kwb.package)

Development: [https://kwb-r.github.io/kwb.package/dev](https://kwb-r.github.io/kwb.package/dev)
