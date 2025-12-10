# hmde

`hmde` which stands for **h**ierarchical **m**ethods for
**d**ifferential **e**quations is a package built for biologists with
repeat size measurement data who want to fit a specific set of growth
functions.

## Installation

`hmde` is under active development, you can install the stable,
development version of `hmde` from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("traitecoevo/hmde")
```

## Overview

We treat growth as the continuous rate of change for size, and fit
size-dependent growth functions. The repeat survey data requires
multiple measurements from the same individuals over time that can be
connected up as
$$size\left( t_{j + 1} \right) = size\left( t_{j} \right) + growth\left( {\text{from}\mspace{6mu}}t_{j}{\mspace{6mu}\text{to}\mspace{6mu}}t_{j + 1} \right).$$

We assume that different individuals will have variation in the
specifics of their growth function governed by the function parameters,
but that individuals from the same population will have the same
function description.

### Notation

For notation we will express the true size for individual $i$ at time
$t_{j}$ as $Y_{i}\left( t_{j} \right)$, the growth function as $f$, and
a parameter of individual $i$ as $\beta_{i}$. So Equation can be
expressed as
$$Y_{i}\left( t_{j + 1} \right) = Y_{i}\left( t_{j} \right) + \int_{t_{j}}^{t_{j + 1}}f\left( Y(t),\beta_{i} \right)\, dt$$
where the integral adds up all the growth over the intervening time.
Each model we use will comes with its specific growth parameters that we
will describe. Some are more biologically interpretable than others. We
don’t assume that we see the true sizes, and instead have observed size
$$y_{ij} = Y_{i}\left( t_{j} \right) + {\mspace{6mu}\text{error}}.$$ We
have assumed normally distributed error in `hmde`,this has proven
reasonably robust in simulation for a more general size-dependent error
model. For details see O’Brien, Warton, and Falster (2024).

Due to the hierarchical structure of the statistical model, we have
distributions that govern the behaviour of growth parameters. If we are
modeling only a single individual, we don’t worry about the underlying
distribution so much. If we have multiple individuals then we have a
distribution with hyper-parameters that acts as a population-level
feature, so $$\beta_{i} \sim \log\mathcal{N}(\mu,\sigma)$$ for example,
and we can examine the behaviour of the mean and standard deviation as
population-level features.

## `hmde` supported growth functions

- Constant growth
- Von Bertalanffy
- Canham

For each growth function, we have implementation to model the growth of
a single or for multiple individuals.

## Workflow

Broadly, the workflow for `hmde` is to:

1.  Wrangle the data into the required format for a chosen model,
2.  Pass the data and the model to a function that runs the sampling
    with Stan’s MCMC algorithms.
3.  Inspect and analysis the returned `Stan` fit object.

We will demonstrate this workflow using case studies that uses the three
growth functions that are supported in `hmde`. You can find these on our
website or you can view these in R using:

``` r
vignettes("constant-growth")
vignettes("von-bertalanffy")
vignettes("canham")
```

For each case study, we will discuss why that growth function was chosen
in the context of the survey process as data availability is a key
factor in determining which functions can be used. We will not discuss
the mathematical and statistical theory in depth, if that is of
interest, check out the vignette ‘hmde for Mathematicians’ or check out
the methodology paper O’Brien, Warton, and Falster (2024).

## References

O’Brien, Tess, David Warton, and Daniel Falster. 2024. “Yes, They’re All
Individuals: Hierarchical Models for Repeat Survey Data Improve
Estimates of Tree Growth and Sie.” *Methods in Ecology and Evolution*.
