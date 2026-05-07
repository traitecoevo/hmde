# Run chosen pre-built model in Stan

Run chosen pre-built model in Stan

## Usage

``` r
hmde_run(data_template, ...)
```

## Arguments

- data_template:

  hmde_data_template class object

- ...:

  additional arguments passed to rstan::sampling

## Value

Stanfit S4 object

## Examples

``` r
# basic usage of hmde_run
hmde_data_template("constant_single_ind",
                   obs_data = Trout_Size_Data[1:4,]) |>
  hmde_run(chains = 1, iter = 1000,
           verbose = FALSE, show_messages = FALSE)
#> 
#> SAMPLING FOR MODEL 'constant_single_ind' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 8e-06 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.08 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
#> Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
#> Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
#> Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
#> Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.01 seconds (Warm-up)
#> Chain 1:                0.008 seconds (Sampling)
#> Chain 1:                0.018 seconds (Total)
#> Chain 1: 
#> Warning: There were 1 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is NA, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#> Inference for Stan model: constant_single_ind.
#> 1 chains, each with iter=1000; warmup=500; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=500.
#> 
#>                                         mean se_mean   sd  2.5%   25%   50%
#> ind_y_0                                51.61    0.17 1.38 48.50 51.12 51.62
#> ind_beta                                4.60    0.05 0.41  3.22  4.49  4.64
#> global_error_sigma                      1.18    0.12 1.01  0.33  0.57  0.84
#> y_hat[1]                               51.61    0.17 1.38 48.50 51.12 51.62
#> y_hat[2]                               60.40    0.07 0.85 58.05 60.13 60.48
#> y_hat[3]                               70.08    0.08 0.92 67.80 69.87 70.23
#> y_hat[4]                               79.35    0.18 1.52 74.70 79.01 79.55
#> check_prior_pars_ind_beta[1]            0.00     NaN 0.00  0.00  0.00  0.00
#> check_prior_pars_ind_beta[2]            2.00     NaN 0.00  2.00  2.00  2.00
#> check_prior_pars_global_error_sigma[1]  0.00     NaN 0.00  0.00  0.00  0.00
#> check_prior_pars_global_error_sigma[2]  2.00     NaN 0.00  2.00  2.00  2.00
#> lp__                                    0.80    0.31 2.26 -4.96 -0.19  1.47
#>                                          75% 97.5% n_eff Rhat
#> ind_y_0                                52.18 55.14    69 1.00
#> ind_beta                                4.75  5.26    56 1.03
#> global_error_sigma                      1.37  3.93    75 1.03
#> y_hat[1]                               52.18 55.14    69 1.00
#> y_hat[2]                               60.83 61.71   131 1.00
#> y_hat[3]                               70.51 71.46   132 1.04
#> y_hat[4]                               80.04 81.51    74 1.05
#> check_prior_pars_ind_beta[1]            0.00  0.00   NaN  NaN
#> check_prior_pars_ind_beta[2]            2.00  2.00   NaN  NaN
#> check_prior_pars_global_error_sigma[1]  0.00  0.00   NaN  NaN
#> check_prior_pars_global_error_sigma[2]  2.00  2.00   NaN  NaN
#> lp__                                    2.42  3.38    55 1.06
#> 
#> Samples were drawn using NUTS(diag_e) at Thu May  7 00:05:19 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```
