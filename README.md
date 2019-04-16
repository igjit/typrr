# typrr

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/igjit/typrr.svg?branch=master)](https://travis-ci.org/igjit/typrr)
<!-- badges: end -->

typrr is a toy type checker for R.

## Installation

You can install typrr from github with:

``` r
# install.packages("devtools")
devtools::install_github("igjit/typrr")
```

## Example

``` r
typrr::type_check("filename.R")
```
