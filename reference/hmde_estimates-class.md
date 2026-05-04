# hmde_estimates class An S4 class to contain model information and parameter estimates for a hmde model.

hmde_estimates class An S4 class to contain model information and
parameter estimates for a hmde model.

model_name getter Getter for model_name in hmde_estimates object

model_name setter Setter for model_name in hmde_estimates object

model_level getter Getter for model_level in hmde_estimates object

model_level settter Setter for model_level in hmde_estimates object

method getter Getter for method in hmde_estimates object

method setter Setter for method in hmde_estimates object

runtime getter Getter for runtime in hmde_estimates object

runtime setter Setter for runtime in hmde_estimates object

fit_summary getter Getter for fit_summary in hmde_estimates object

fit_summary setter Setter for fit_summary in hmde_estimates object

measurement_ests getter Getter for measurement_ests in hmde_estimates
object

measurement_ests setter Setter for measurement_ests in hmde_estimates
object

individual_ests getter Getter for individual_ests in hmde_estimates
object

individual_ests setter Setter for individual_ests in hmde_estimates
object

population_ests getter Getter for population_ests in hmde_estimates
object

population_ests setter Setter for population_ests in hmde_estimates
object

Getter for error_ests in hmde_estimates object

Setter for error_ests in hmde_estimates object

Getter for prior_pars in hmde_estimates object

Setter for prior_pars in hmde_estimates object

par_names getter Getter for par_names in hmde_estimates object

par_names setter Setter for par_names in hmde_estimates object

Constructor function for hmde_estimates class.

Show function for hmde_estimates object

Print function for hmde_estimates object

Summary function for hmde_estimates object

Plot function for hmde_estimates object

## Usage

``` r
# S4 method for class 'hmde_estimates'
model_name(x)

# S4 method for class 'hmde_estimates'
model_name(x) <- value

# S4 method for class 'hmde_estimates'
model_level(x)

# S4 method for class 'hmde_estimates'
model_level(x) <- value

# S4 method for class 'hmde_estimates'
method(x)

# S4 method for class 'hmde_estimates'
method(x) <- value

# S4 method for class 'hmde_estimates'
runtime(x)

# S4 method for class 'hmde_estimates'
runtime(x) <- value

# S4 method for class 'hmde_estimates'
fit_summary(x)

# S4 method for class 'hmde_estimates'
fit_summary(x) <- value

# S4 method for class 'hmde_estimates'
measurement_ests(x)

# S4 method for class 'hmde_estimates'
measurement_ests(x) <- value

# S4 method for class 'hmde_estimates'
individual_ests(x)

# S4 method for class 'hmde_estimates'
individual_ests(x) <- value

# S4 method for class 'hmde_estimates'
population_ests(x)

# S4 method for class 'hmde_estimates'
population_ests(x) <- value

# S4 method for class 'hmde_estimates'
error_ests(x)

# S4 method for class 'hmde_estimates'
error_ests(x) <- value

# S4 method for class 'hmde_estimates'
prior_pars(x)

# S4 method for class 'hmde_estimates'
prior_pars(x) <- value

# S4 method for class 'hmde_estimates'
par_names(x)

# S4 method for class 'hmde_estimates'
par_names(x) <- value

hmde_estimates(fit, obs_data)

# S4 method for class 'hmde_estimates'
show(object)

# S4 method for class 'hmde_estimates'
print(x)

# S4 method for class 'hmde_estimates'
summary(object)

# S4 method for class 'hmde_estimates,ANY'
plot(x)
```

## Arguments

- x:

  hmde_estimates class object

- value:

  vector

- fit:

  stanfit class object

- obs_data:

  tbl_df class object with variables for ind_id, time, y_obs

- object:

  hmde_estimates object to be validated

## Value

hmde_estimates class object

## Slots

- `model_name`:

  name of the hmde model

- `model_level`:

  whether the model functions at the single or multi-ind level

- `method`:

  sampling method eg. MCMC

- `runtime`:

  matrix of chain runtime

- `fit_summary`:

  description of the fit

- `measurement_ests`:

  tibble of measurement-level estimates

- `individual_ests`:

  tibble of individual-level estimates

- `population_ests`:

  list of population_level estimates

- `error_ests`:

  tibble of error parameter estimates

- `prior_pars`:

  list of prior parameters

- `par_names`:

  list of model paramter names at each level

## Examples

``` r
# basic usage of hmde_estimates
hmde_data_template("constant_single_ind",
                   obs_data = Trout_Size_Data[1:4,]) |>
  hmde_run(chains = 1, iter = 1000,
           verbose = FALSE, show_messages = FALSE) |>
  hmde_estimates(obs_data = Trout_Size_Data[1:4,])
#> 
#> SAMPLING FOR MODEL 'constant_single_ind' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2.3e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.23 seconds.
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
#> Chain 1:  Elapsed Time: 0.011 seconds (Warm-up)
#> Chain 1:                0.008 seconds (Sampling)
#> Chain 1:                0.019 seconds (Total)
#> Chain 1: 
#> Warning: The largest R-hat is NA, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#> Model name: constant_single_ind
#> Model level: single individual
#> Top level: individual
#> Top level parameter estimates:
#> Method: MCMC sampling with NUTS algorithm
#> Chains: 1
#> Iterations: 1000
#> Warmup: 500
#> 
#> Top level parameter estimates:
#> | ind_id| ind_beta_mean| ind_beta_median| ind_beta_CI_lower| ind_beta_CI_upper|
#> |------:|-------------:|---------------:|-----------------:|-----------------:|
#> |      1|         4.595|            4.63|              3.87|             5.142|
#> 
#> Runtime information:
#> |        | warmup| sample|
#> |:-------|------:|------:|
#> |chain:1 |  0.011|  0.008|
```
