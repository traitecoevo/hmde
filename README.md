hmde: Hierarchicam Methods for Differential Equations
================

The goal of `hmde` is to implement hierarchical Bayesian longitudinal
models to solve the Bayesian inverse problem of estimating differential
equation parameters based on repeat measurement surveys. Estimation is
done using Markov Chain Monte Carlo, implemented through
[Stan](https://mc-stan.org/) via
[RStan](https://mc-stan.org/users/interfaces/rstan), built under
[R](https://cran.r-project.org/) 4.3.3. The inbuilt models are based on
case studies in ecology. The hierarchical Bayesian longitudinal method
was first introduced in O’Brien et al.,
[2024](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14463).

As `hmde` is first intended for biologists, the initial set of vignettes
(`hmde`, `constant-growth`, `von-bertalanffy`, and `canham`) are written
aimed at an audience more interested in applications than the underlying
theory. A vignette for the more mathematically interested is under
development.

## The Maths

The general use case is to estimate a vector of parameters
$\boldsymbol{\theta}$ for a chosen differential equation
$$f\left( Y \left( t \right), \boldsymbol{\theta} \right) = \frac{dY}{dt}$$
based on the longitudinal structure
$$Y \left( t_{j+1} \right) = Y\left( t_j \right) + \int_{t_j}^{t_{j+1}}f\left( Y \left( t \right), \boldsymbol{\theta} \right)\,dt. $$

The input data are observations of the form $y_{ij}$ for individual $i$
at time $t_j$, with repeated observations coming from the same
individual. We parameterise $f$ at the individual level by estimating
$\boldsymbol{\theta}_i$ as the vector of parameters. We have
hyper-parameters that determine the distribution of
$\boldsymbol{\theta}_i$ with typical prior distribution
$$\boldsymbol{\theta}_i \sim \log \mathcal{N}\left(\boldsymbol{\mu}_{\log\left(\boldsymbol{\theta}\right)}, \boldsymbol{\sigma}_{\log \left( \boldsymbol{\theta} \right)}\right), $$
where $\boldsymbol{\mu}_{\log\left(\boldsymbol{\theta}\right)}$ and
$\boldsymbol{\sigma}_{\log\left(\boldsymbol{\theta}\right)}$ are vectors
of means and standard deviations. In the case of a single individual,
these are chosen prior values. In the case of a multi-individual model
$\boldsymbol{\mu}_{\log\left(\boldsymbol{\theta}\right)}$ and
$\boldsymbol{\sigma}_{\log\left(\boldsymbol{\theta}\right)}$ have their
own prior distributions and are fit to data.

## Implemented Models

`hmde` comes with four DEs built and ready to go, each of which has a
version for a single individual and multiple individuals.

### Constant Model

The constant model is given by
$$f \left( Y \left( t \right), \beta \right) = \frac{dY}{dt} = \beta,$$
and is best understood as describing the average rate of change over
time.

### von Bertalanffy

The von Bertalanffy mode is given by
$$f \left( Y \left( t \right), \beta, Y_{max} \right) = \frac{dY}{dt} = \beta \left( Y_{max} - Y \left( t \right) \right),$$
where $\beta$ is the growth rate parameter and $Y_{max}$ is the maximum
value that $Y$ takes.

### Canham

The Canham ([Canham et
al. 2004](https://doi.org/10.1890/1051-0761(2006)016%5B0540:NAOCTC%5D2.0.CO;2))
model is a hump-shaped function given by
$$f \left( Y \left( t \right), f_{max}, Y_{max}, k \right) = \frac{dY}{dt} = f_{max} \exp \Bigg( -\frac{1}{2} \bigg( \frac{ \ln \left( Y \left( t \right) / Y_{max} \right) }{k} \bigg)^2 \Bigg), $$
where $f_{max}$ is the maximum growth rate, $Y_{max}$ is the $Y$-value
at which that maximum occurs, and $k$ controls how narrow or wide the
peak is.

## 

## Installation

‘hmde’ is under active development. You can install the current
developmental version of ‘hmde’ from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("traitecoevo/hmde")
```

## Quick demo

Create constant growth data with measurement error:

``` r
library(hmde)
y_obs <- seq(from=2, to=15, length.out=10) + rnorm(10, 0, 0.1)
```

Measurement error is necessary as otherwise the normal likelihood
$$s_{ij} \sim \mathcal{N}\left( 0, \sigma_e \right)$$ blows up as
$\sigma_e$ approaches 0.

Fit the model:

``` r
constant_fit <- hmde_model("constant_single_ind") |>
        hmde_assign_data(n_obs = 10,                  #Integer
                         y_obs = y_obs,               #vector length n_obs
                         obs_index = 1:10,            #vector length n_obs
                         time = 0:9,                  #Vector length n_obs
                         y_0_obs = y_obs[1]           #Real
        ) |>
        hmde_run(chains = 1, iter = 1000, verbose = FALSE, show_messages = FALSE)
```

## Found a bug?

Please submit a [GitHub
issue](https://github.com/traitecoevo/hmde/issues) with details of the
bug. A [reprex](https://reprex.tidyverse.org/) would be particularly
helpful with the bug-proofing process!
