
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmot

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/rmot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/traitecoevo/rmot/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ‘rmot’ is to …

## Installation

You can install the development version of ‘rmot’ from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("traitecoevo/rmot")
```

## Demo

``` r
rmot_model("linear") |>
  rmot_assign_data(X = Loblolly$age,
                   Y = Loblolly$height,
                   N = nrow(Loblolly)) |>
  rmot_run()
```

## Found a bug?

Please lodge a GitHub Issue with details of the bug
