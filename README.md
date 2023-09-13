# mpindex

<!-- badges: start -->
<div style="display: flex; align-items: center; ">
  <a style="display: inline-flex;" href="https://cran.r-project.org/package=mpindex" target="_blank">
    <img src="https://www.r-pkg.org/badges/version/mpindex" alt="CRAN Status" />
  </a> 
  <a style="display: inline-flex;" href="https://github.com/yng-me/mpindex/actions/workflows/R-CMD-check.yaml" target="_blank">
  <img src="https://github.com/yng-me/mpindex/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check" />
  </a>
  <a style="display: inline-flex;" href="https://app.codecov.io/gh/yng-me/mpindex?branch=main" target="_blank">
    <img src="https://codecov.io/gh/yng-me/mpindex/branch/main/graph/badge.svg" alt="codecov" />
  </a>
</div>
<!-- badges: end -->

`mpindex` offers a set of easy-to-use functions for computing Multidimensional Poverty Index (MPI) using the Alkire-Foster (AF) counting method developed by Sabina Alkire and James Foster. Learn more in `vignette("mpindex")` and `?compute_mpi`.

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
