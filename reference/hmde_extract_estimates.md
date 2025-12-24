# Extract samples and return measurement, individual, and population-level estimates

Extract samples and return measurement, individual, and population-level
estimates

## Usage

``` r
hmde_extract_estimates(fit = NULL, input_measurement_data = NULL)
```

## Arguments

- fit:

  fitted model Stan fit

- input_measurement_data:

  data used to fit the model with ind_id, y_obs, time, obs_index tibble

## Value

named list with data frames for measurement, individual,
population-level, and error parameter estimates

## Examples

``` r
# basic usage of hmde_extract_estimates
hmde_model("constant_single_ind") |>
  hmde_assign_data(Trout_Size_Data)|>
  hmde_run(chains = 1, iter = 1000,
           verbose = FALSE, show_messages = FALSE) |>
  hmde_extract_estimates(Trout_Size_Data)
#> 
#> SAMPLING FOR MODEL 'constant_single_ind' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2.1e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.21 seconds.
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
#> Chain 1:  Elapsed Time: 0.043 seconds (Warm-up)
#> Chain 1:                0.032 seconds (Sampling)
#> Chain 1:                0.075 seconds (Total)
#> Chain 1: 
#> $model_name
#> [1] "constant_single_ind"
#> 
#> $measurement_data
#> # A tibble: 135 × 5
#>    ind_id  time y_obs obs_index y_hat
#>     <dbl> <dbl> <dbl>     <dbl> <dbl>
#>  1      1  0       52         1  65.9
#>  2      1  1.91    60         2  71.8
#>  3      1  4.02    70         3  78.4
#>  4      1  6.04    80         4  84.7
#>  5      2  0       80         1  65.9
#>  6      2  1.90    85         2  71.8
#>  7      2  3.94    93         3  78.1
#>  8      2  5.96    94         4  84.4
#>  9      3  0       52         1  65.9
#> 10      3  2.03    65         2  72.2
#> # ℹ 125 more rows
#> 
#> $individual_data
#> # A tibble: 1 × 5
#>   ind_id ind_beta_mean ind_beta_median ind_beta_CI_lower ind_beta_CI_upper
#>    <int>         <dbl>           <dbl>             <dbl>             <dbl>
#> 1      1          3.11            3.11              2.25              3.99
#> 
#> $error_data
#> # A tibble: 1 × 5
#>   par_name            mean median CI_lower CI_upper
#>   <chr>              <dbl>  <dbl>    <dbl>    <dbl>
#> 1 global_error_sigma  11.1   11.1     9.96     12.3
#> 
```
