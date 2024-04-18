
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmot

<!-- badges: start -->

[![R-CMD-check](https://github.com/traitecoevo/rmot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/traitecoevo/rmot/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/traitecoevo/rmot/branch/master/graph/badge.svg)](https://app.codecov.io/gh/traitecoevo/rmot?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rmot is to implement hierarchical Bayesian longitudinal
models to solve the Bayesian inverse problem of estimating differential
equation parameters based on repeat measurement surveys. Estimation is
done through Markov Chain Monte Carlo, implemented through Stan (CITE)
via RStan (CITE), build under R 4.3.3 (CITE). The inbuilt models are
based on a case study in tropical tree growth behaviour.

## The Maths

The general use case is to estimate a vector of parameters
$\boldsymbol{\theta}$ for a chosen differential equation
$$ f(Y(t), \boldsymbol{\theta}) = \frac{dY}{dt}$$ based on the
longitudinal structure
$$ Y(t_{j+1}) = Y(t_j) + \int_{t_j}^{t_{j+1}}f(Y(t), \boldsymbol{\theta})\,dt.
$$

The input data are observations of the form $y_{ij}$ for individual $i$
at time $t_j$, with repeated observations coming from the same
individual. We parameterise $f$ at the individual level by estimating
$\boldsymbol{\theta}_i$ as the vector of parameters. We have
hyper-parameters that determine the distribution of
$\boldsymbol{\theta}_i$ with typical prior distribution
$$\boldsymbol{\theta}_i \sim \log \mathcal{N}(\boldsymbol{\mu}_{\log(\boldsymbol{\theta})}, \boldsymbol{\sigma}_{\log(\boldsymbol{\theta})}),
$$ where $\boldsymbol{\mu}_{\log(\boldsymbol{\theta})}$ and
$\boldsymbol{\sigma}_{\log(\boldsymbol{\theta})}$ are vectors of means
and standard deviations. In the case of a single individual, these are
chosen prior values. In the case of a multi-individual model
$\boldsymbol{\mu}_{\log(\boldsymbol{\theta})}$ and
$\boldsymbol{\sigma}_{\log(\boldsymbol{\theta})}$ have their own prior
distributions and are fit to data.

## Implemented Models

Rmot comes with four DEs built and ready to go, each of which has a
version for a single individual and multiple individuals.

### Constant Model

The constant model is given by
$$ f(Y(t), \beta) = \frac{dY}{dt} = \beta,$$ and is best understood as
describing the average rate of change over time.

### Power law

The power law model is given by
$$ f(Y(t), \beta_0, \beta_1, \bar{Y}) = \frac{dY}{dt} = \beta_0 \bigg(\frac{Y(t)}{\bar{Y}}\bigg)^{\beta_1},
$$ where $\beta_0>0$ is the coefficient, $\beta_1$ is the power, and
$\bar{Y}$ is a user-provided parameter that centres the model in order
to avoid correlation between the $\beta$s.

### von Bertalanffy

The von Bertalanffy mode is given by
$$ f(Y(t), \beta, Y_{max}) = \frac{dY}{dt} = \beta (Y_{max} - Y(t)),
$$ where $\beta$ is the growth rate parameter and $Y_{max} > 0$ is the
maximum value that $Y$ takes.

### Canham

The Canham model is a hump-shaped function given by
$$ f(Y(t), f_{max}, Y_{max}, k) = \frac{dY}{dt} = f_{max} \exp \Bigg(-\frac{1}{2}\bigg(\frac{\ln(Y(t)/Y_{max})}{k} \bigg)^2 \Bigg), 
$$ where $f_{max}$ is the maximum growth rate, $Y_{max}$ is the
$Y$-value at which that maximum occurs, and $k$ controls how narrow or
wide the peak is.

## 

## Installation

‘rmot’ is under active development. You can install the current
developmental version of ‘rmot’ from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("traitecoevo/rmot")
```

## Quick demo

Create constant growth data with measurement error.

``` r
y_obs <- seq(from=2, to=15, length.out=10) + rnorm(10, 0, 0.1)
```

Fit the model.

``` r
constant_fit <- rmot_model("constant_single_ind") |>
        rmot_assign_data(n_obs = 10,                                #Integer
                         y_obs = y_obs, #vector length n_obs
                         obs_index = 1:10,                          #vector length n_obs
                         time = 0:9,                                #Vector length n_obs
                         y_0_obs = 2                                #Real
        ) |>
        rmot_run(chains = 1, iter = 1000, verbose = FALSE, show_messages = FALSE)
```

## Found a bug?

Please submit a [GitHub
issue](https://github.com/traitecoevo/rmot/issues) with details of the
bug. A [reprex](https://reprex.tidyverse.org/) would be particularly
helpful with the bug-proofing process!
