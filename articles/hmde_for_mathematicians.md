# hmde for Mathematicians

This vignette is intended for a statistical/mathematical audience who
are interested in Bayesian inverse problems. For biologists looking for
an applications-based walkthrough the other vignettes in this package –
`hmde`, `constant-growth`, `von-bertalanffy`, and `canham` – are less
theoretical.

## Getting started with {hmde}

‘hmde’ is under active development, you can install the development
version of ‘hmde’ from [GitHub](https://github.com/) with:

``` r

# install.packages("remotes")
remotes::install_github("traitecoevo/hmde")
```

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(hmde)
```

## The Theory

The underlying method that the hmde package implements leverages the
longitudinal structure of repeat measurement data to estimate parameters
for an underlying differential equation, as first demonstrated in . This
method is an example of a Bayesian inverse method for differential
equation estimation: we are attempting to estimate the parameters of a
chosen DE based on observations of the resulting process. We assume that
the data consists of a finite number of discrete (likely sparse)
observations with measurement error at the individual level, and have a
set number of possible differential equations ready to be fit to data.

### The Maths

In a general setting, we are interested in some quantity $`Y(t)`$ that
changes over time (approximately) according to a chosen DE $`f`$. We
have some finite finite number measurements at times $`t_j`$, and
believe that the underlying true behaviour is given by
``` math
Y(t_{j+1}) = Y(t_j) + \int_{t_j}^{t_{j+1}} f(Y(t), \boldsymbol{\theta})\,dt\qquad (1)
```
for unseen parameter vector $`\boldsymbol{\theta}`$ that we wish to
estimate. We also have an initial condition $`Y_0 = Y(t_0)`$.

We have three in-built growth functions in `hmde`.

- Constant: $`f = \beta`$ chosen as when you only have two observations
  the average growth rate is the best you can do. Furthermore, the
  results from the constant model align with a linear mixed effects
  model for size with individual parameter. The constant model is
  size-independent. As all numerical methods are equivalent and the same
  as the analytic solution given the initial condition, we use the Euler
  method for the constant model.

- von Bertalanffy:
  ``` math
  f = \beta (Y_{max} - Y(t))\qquad\qquad(2)
  ```
  which has a history of use in biology for species that grow to some
  maximum size, and represents a simple size-dependent linear function.
  If a power law model is desired, log-transforming observations and
  then back-transforming by exponentiation gives such a function.
  Equation (2) is implemented with the analytic solution rather than a
  numerical method, as it is a known nightmare example for numerical
  stability due to the negative coefficient on $`Y(t)`$.

- Canham:
  ``` math
  f = f_{max} \exp\Bigg(-\frac{1}{2}\bigg(\frac{\log(Y(t)/Y_{max})}{k} \bigg)^2 \Bigg),\qquad\qquad(3)
  ```
  which is considered a reasonable approximation of long term growth
  behaviour for some tree species as shown in and \[Chapter 2\].
  Equation (3) is extremely non-linear and does not have an analytic
  solution, forcing the use of numerical methods in order to estimate
  the growth increments in Equation (1).

- Affine:
  ``` math
  f = \beta_0 - \beta_1 Y(t)
  ```
  which is only included for demonstration purposes of where numerical
  methods can go wrong as it is a re-parameterisation of the von
  Bertalanffy model.

Choice of appropriate function is an exercise for the user and depends
on the available data. Aside from the affine model all have versions
that work with both a single individual, and multiple individuals. We
provide example data intended for use with each of the primary models: -
`Trout_Size_Data` for the constant model, - `Lizard_Size_Data` for the
von Bertalanffy model, - `Tree_Size_Data` for the Canham model.

### The Stats

We assume that we do not have access to $`\boldsymbol{\theta}`$ or the
true values of $`Y`$ over time. Instead we have observations with
measurement error,
``` math
y_j = Y(t_j) + \text{error},
```
and estimate $`\boldsymbol{\theta}`$ with $`\hat{\boldsymbol{\theta}}`$.

We use a hierarchical structure to encode different levels of
relationships within the data. At the bottom of the hierarchy is the
measurement level
``` math
y_j \sim \mathcal{N}(\hat{Y}(t_j), \sigma_e),
```
where we assume normally distributed error. This may not always be true,
but indicated that symmetric error centred at 0 may be enough for
reasonable results. The longitudinal structure in Equation (1) serves as
the next level, connecting estimated sizes over time based on the chosen
function $`f`$ and estimated parameters $`\hat{\boldsymbol{\theta}}`$,
which operates at the level of the individual.

If the data has multiple individuals we add additional layers that act
as hyper-parameters on the distributions of elements of each individual
$`i`$’s $`\hat{\boldsymbol{\theta}}_i`$. We build these to be
independent $`\theta_{ki}`$s, with log-mean and log-standard deviation
parameters
``` math
\theta_k \sim \log\mathcal{N}(\mu_k, \sigma_k),
```
and typically use the following priors:
``` math
\mu_k \sim\mathcal{N}(0,2), \quad 0 < \sigma_k\sim Cauchy(0, 2).
```
For details on the prior distributions see the vignette for a specific
model or check the Stan file. To see the default values run we look at
the prior parameters for a given model:

``` r

prior_pars(hmde_model("canham_multi_ind"))
#> $prior_pars_pop_log_max_growth_mean
#> [1] 0 2
#> 
#> $prior_pars_pop_log_max_growth_sd
#> [1] 0 2
#> 
#> $prior_pars_pop_log_size_at_max_growth_mean
#> [1] 0 2
#> 
#> $prior_pars_pop_log_size_at_max_growth_sd
#> [1] 0 2
#> 
#> $prior_pars_pop_log_k_mean
#> [1] 0 2
#> 
#> $prior_pars_pop_log_k_sd
#> [1] 0 2
#> 
#> $prior_pars_global_error_sigma
#> [1] 0 2
```

The error parameter $`\sigma_e >0`$ is assumed to operate at a global
level independent of individual and typically has a Cauchy prior with
location 0, spread parameter 2.

Estimation is currently done using MCMC through Stan. A deterministic
method such as joint max posterior likelihood is intended as an option
in the future.

### Integration of time series

Numerical methods are required for the Canham model as it has no
analytic solution, and the inbuilt Stan Runge-Kutta 4-5 solver is used.

For the von Bertalanffy model an analytic solution is used in order to
avoid numerical problems. For the constant model all numerical methods
are the same and give the same result as the analytic solution so Euler
is used.

## Demonstration: Canham Growth - Multiple Individuals

The provided tree data for 50 individuals takes a few hours to run. As
such, the following block does not run by default, and instead we
leverage the provided estimates data file: `Tree_Size_Ests`. The
estimates are posterior mean, median, and 95% central credible intervals
for parameters, and mean posterior estimates of sizes over time based on
the longitudinal model.

``` r

# Build fit and extract estimates
set.seed(2026)
canham_multi_ind_fit <- hmde_data_template("canham_multi_ind",
                                           obs_data = Tree_Size_Data) |>
  hmde_run(chains = 1, cores = 1, iter = 1000)

Tree_Size_Ests <- hmde_estimates(fit = canham_multi_ind_fit,
                                 obs_data = Tree_Size_Data)
```

The following code produces plots of the Canham function for each
individual between the first and last estimated size. The purpose of the
plot is to look at how different individual growth functions behave.

``` r

summary(Tree_Size_Ests)
#> Model name: canham_multi_ind
#> Model level: multi-individual
#> Top level: population
#> Method: MCMC sampling with NUTS algorithm
#> Chains: 4
#> Iterations: 2000
#> Warmup: 1000
#> 
#> Top level parameter estimates:
#> |par_name                        |   mean| median| CI_lower| CI_upper|
#> |:-------------------------------|------:|------:|--------:|--------:|
#> |pop_log_max_growth_mean         | -1.261| -1.213|   -1.615|   -0.996|
#> |pop_log_max_growth_sd           |  0.416|  0.412|    0.285|    0.563|
#> |pop_log_size_at_max_growth_mean |  2.326|  2.332|    0.986|    3.286|
#> |pop_log_size_at_max_growth_sd   |  0.546|  0.382|    0.119|    3.166|
#> |pop_log_k_mean                  | -0.119| -0.511|   -0.863|    3.066|
#> |pop_log_k_sd                    |  0.524|  0.538|    0.131|    0.857|

# Plot fitted growth function pieces
hmde_plot_de_pieces(Tree_Size_Ests)
```

![](hmde_for_mathematicians_files/figure-html/unnamed-chunk-6-1.png)
Each line represents 25 years of growth for the specific individual.
Lines that sit lower on the $`y`$-axis are shorter horizontally because
they are traversed more slowly, as $`f`$ is the rate of change in $`Y`$.

We have provided a function to plot observed and estimated sizes for a
selected number of individuals:

``` r

#Plots of size over time for a sample of 5 individuals
hmde_plot_obs_est_inds(Tree_Size_Ests, n_ind_to_plot = 5)
```

![](hmde_for_mathematicians_files/figure-html/unnamed-chunk-7-1.png)
