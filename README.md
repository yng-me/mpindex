# mpindex

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/mpindex)](https://CRAN.R-project.org/package=mpindex)
[![R-CMD-check](https://github.com/yng-me/mpindex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yng-me/mpindex/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/yng-me/mpindex/graph/badge.svg)](https://app.codecov.io/gh/yng-me/mpindex)
<!-- badges: end -->

Estimate **Multidimensional Poverty Index (MPI)** measures from household survey microdata using the [Alkire-Foster dual-cutoff counting method](https://doi.org/10.1016/j.jpubeco.2010.11.006). Load your indicator specification from a CSV, Excel, JSON, or plain-text file; compute the headcount ratio (H), intensity (A), and MPI = H × A; disaggregate by any subgroup; and export results to a formatted Excel report. Complex survey designs — stratification, clustering, and probability weights — are supported out of the box, with optional design-based standard errors and confidence intervals.

Learn more in `vignette("mpindex")` and `?compute_mpi`.

> **Upgrading from 0.2.x?** See the [migration guide](https://yng-me.github.io/mpindex/articles/migrating-to-0-3-0.html) for a full list of breaking changes and before/after examples.

## Installation

To install the `mpindex` package from CRAN:

```r
install.packages('mpindex')
```

If you want to get the latest development version of `mpindex`, install it from GitHub. Note that you may need to install `devtools`.

```r
# install.packages("devtools")
devtools::install_github('yng-me/mpindex')
```
## Usage

Load the package once you have successfully completed the installation.

```r
library(mpindex)
```
