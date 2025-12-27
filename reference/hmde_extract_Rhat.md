# Calculate Rhat statistics for a hmde_fit object

Calculate Rhat statistics for a hmde_fit object

## Usage

``` r
hmde_extract_Rhat(fit)
```

## Arguments

- fit:

  hmde_fit fitted model object, output of hmde_run

## Value

named vector of Rhat values

## Examples

``` r
# basic usage of hmde_extract_Rhat
hmde_model("constant_single_ind") |>
  hmde_assign_data(Trout_Size_Data)|>
  hmde_run(chains = 2, iter = 1000,
           verbose = FALSE, show_messages = FALSE) |>
  hmde_extract_Rhat()
#> 
#> SAMPLING FOR MODEL 'constant_single_ind' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.2 seconds.
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
#> Chain 1:  Elapsed Time: 0.054 seconds (Warm-up)
#> Chain 1:                0.036 seconds (Sampling)
#> Chain 1:                0.09 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'constant_single_ind' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.2e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:   1 / 1000 [  0%]  (Warmup)
#> Chain 2: Iteration: 100 / 1000 [ 10%]  (Warmup)
#> Chain 2: Iteration: 200 / 1000 [ 20%]  (Warmup)
#> Chain 2: Iteration: 300 / 1000 [ 30%]  (Warmup)
#> Chain 2: Iteration: 400 / 1000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 500 / 1000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 501 / 1000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 600 / 1000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 700 / 1000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 800 / 1000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 900 / 1000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 1000 / 1000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.057 seconds (Warm-up)
#> Chain 2:                0.042 seconds (Sampling)
#> Chain 2:                0.099 seconds (Total)
#> Chain 2: 
#>            ind_y_0           ind_beta global_error_sigma              y_hat 
#>           1.017323           1.007018           1.002807           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.005895           1.008050           1.007499           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.005859           1.008361           1.007821           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.006209           1.009006           1.007800           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.009745           1.017323           1.006080           1.008257 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.007691           1.017323           1.008494           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017322           1.017323           1.005382           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.005514           1.017323           1.005836           1.007864 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.010454           1.017323           1.006283 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.006221           1.008183           1.007821 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.006346           1.017323           1.006637 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.008102           1.017323           1.005823           1.008282 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.006167           1.008432           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.006441           1.017323           1.006158           1.008318 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.006189           1.006197           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.008092           1.017323           1.006283           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.004826           1.017323           1.006085           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.006621           1.017323           1.005699           1.008221 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.008558           1.017323           1.006580           1.008335 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.005990           1.017323           1.007652 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.007452           1.017323           1.008625 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.008276           1.017323           1.008025 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.006189           1.008227           1.007931 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.005109           1.017323           1.009341 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.008482           1.017323           1.006025 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.008462           1.017323           1.006010           1.008573 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.005401           1.008257           1.008516 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.004876           1.017323           1.005573 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.006154           1.017323           1.006827           1.008469 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.006200           1.007897           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.014513           1.017323           1.005901           1.008204 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.017323           1.005791           1.008367           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.005295           1.008354           1.006391           1.017323 
#>              y_hat              y_hat              y_hat              y_hat 
#>           1.005934           1.008175           1.007081           1.017323 
#>              y_hat              y_hat               lp__ 
#>           1.008183           1.007190           1.007485 
```
