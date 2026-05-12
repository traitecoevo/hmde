# Plan: Add Zeide Growth Model to hmde

## Context

The `hmde` package fits growth models to size-over-time data using Bayesian inference and ODE solvers in Stan. It currently supports Canham, Von Bertalanffy, constant, and affine growth models. The Zeide (1993) YD form is an equally plausible alternative for describing sigmoidal diameter growth with biologically interpretable parameters (Lai et al. 2025, Urban Forestry & Urban Greening). It captures hump-shaped growth–diameter relationships and supports life-history tradeoff analysis across species. Adding it follows the exact same pattern as the existing Canham model.

## The Zeide YD Model Equation

```
dD/dt = z(D, a, b, c) = a * D^b * exp[-c * (D - 1)]
```

Parameters (all positive, estimated on log scale):
- `a` — growth rate at 1-cm diameter
- `b` — growth expansion factor (controls acceleration when small)
- `c` — growth decline factor (controls deceleration when large)

## Files to Create

### 1. `inst/stan/zeide_single_ind.stan`
Single-individual Bayesian ODE model. Pattern mirrors `canham_single_ind.stan`:
- `functions` block with `DE(real t, vector y, real a, real b, real c)`:
  ```stan
  vector[size(y)] dydt = a * exp(b * log(y)) .* exp(-c * (y - 1.0));
  ```
- `data` block: `n_obs`, `y_obs[n_obs]`, `obs_index[n_obs]`, `time[n_obs]`, and three prior par arrays `prior_pars_ind_a[2]`, `prior_pars_ind_b[2]`, `prior_pars_ind_c[2]`, `prior_pars_global_error_sigma[2]`
- `parameters`: `ind_y_0`, `ind_a`, `ind_b`, `ind_c`, `global_error_sigma` (all `<lower=0>`)
- `model` block: `ode_rk45` to propagate size; `normal` likelihood; `lognormal` priors for individual params; `cauchy` prior for sigma
- `generated quantities` block: re-compute `y_hat` and echo back all prior pars (matching Canham pattern)

Default priors (based on the paper's informative priors):
- `prior_pars_ind_a = c(0, 0.5)`, `prior_pars_ind_b = c(-1, 0.5)`, `prior_pars_ind_c = c(-1, 0.5)`
- `prior_pars_global_error_sigma = c(0, 2)`

### 2. `inst/stan/zeide_multi_ind.stan`
Multi-individual hierarchical model. Pattern mirrors `canham_multi_ind.stan`:
- Same DE function block
- Additional `data`: `n_ind`, `ind_id[n_obs]`, and six population prior par arrays (mean + sd for each of a, b, c)
- Additional `parameters`: vectorised `ind_a[n_ind]`, `ind_b[n_ind]`, `ind_c[n_ind]`; population-level `pop_log_a_mean`, `pop_log_a_sd`, `pop_log_b_mean`, `pop_log_b_sd`, `pop_log_c_mean`, `pop_log_c_sd`
- Individual priors drawn from population distributions; population priors from input prior pars
- Same `generated quantities` pattern

### 3. `tests/testthat/test-hmde_models_zeide.R`
Three test groups matching `test-hmde_models_canham.R`:
1. Model structure — `expect_true(inherits(hmde_model("zeide_single_ind"), "hmde_data_template"))`
2. Data assignment — load `Trout_Size_Data`, assign via `hmde_data_template`, check template created
3. Execution — minimal Stan run (chains=1, iter=1) for both single and multi variants

## Files to Modify

### `R/hmde_model_names.R`
Add `"zeide_single_ind"` and `"zeide_multi_ind"` to the returned vector.

### `R/hmde_model_des.R`
Add DE function:
```r
hmde_zeide_de <- function(y = NULL, pars = NULL) {
  return(pars[[1]] * y^pars[[2]] * exp(-pars[[3]] * (y - 1)))
}
```
Add two switch cases mapping both model names to `hmde_zeide_de`.

### `R/hmde_model_pars.R`
Add:
```r
hmde_zeide_single_ind_pars <- function() {
  list(measurement_pars_names = c("y_hat"),
       individual_pars_names = c("ind_a", "ind_b", "ind_c"),
       error_pars_names = c("global_error_sigma"),
       model = "zeide_single_ind")
}

hmde_zeide_multi_ind_pars <- function() {
  list(measurement_pars_names = c("y_hat"),
       individual_pars_names = c("ind_a", "ind_b", "ind_c"),
       population_pars_names = c("pop_log_a_mean", "pop_log_a_sd",
                                 "pop_log_b_mean", "pop_log_b_sd",
                                 "pop_log_c_mean", "pop_log_c_sd"),
       error_pars_names = c("global_error_sigma"),
       model = "zeide_multi_ind")
}
```
Add two switch cases.

### `R/hmde_model.R`
Add constructor functions:
```r
hmde_zeide_single_ind <- function() {
  new("hmde_data_template",
    model_name = "zeide_single_ind",
    obs_data = list(n_obs, y_obs, obs_index, time),
    prior_pars = list(
      prior_pars_ind_a = c(0, 0.5),
      prior_pars_ind_b = c(-1, 0.5),
      prior_pars_ind_c = c(-1, 0.5),
      prior_pars_global_error_sigma = c(0, 2)
    ),
    par_names = hmde_model_pars("zeide_single_ind"))
}

hmde_zeide_multi_ind <- function() {
  new("hmde_data_template",
    model_name = "zeide_multi_ind",
    obs_data = list(n_obs, n_ind, y_obs, obs_index, time, ind_id),
    prior_pars = list(
      prior_pars_pop_log_a_mean = c(0, 0.5),
      prior_pars_pop_log_a_sd = c(0, 1),
      prior_pars_pop_log_b_mean = c(-1, 0.5),
      prior_pars_pop_log_b_sd = c(0, 1),
      prior_pars_pop_log_c_mean = c(-1, 0.5),
      prior_pars_pop_log_c_sd = c(0, 1),
      prior_pars_global_error_sigma = c(0, 2)
    ),
    par_names = hmde_model_pars("zeide_multi_ind"))
}
```
Add two switch cases to the model dispatch.

### `R/hmde_run.R`
In `hmde_stan_names()`, add:
```r
zeide_single_ind = c("n_obs", "y_obs", "obs_index", "time",
                     "prior_pars_ind_a", "prior_pars_ind_b", "prior_pars_ind_c",
                     "prior_pars_global_error_sigma"),
zeide_multi_ind  = c("n_obs", "n_ind", "y_obs", "obs_index", "time", "ind_id",
                     "prior_pars_pop_log_a_mean", "prior_pars_pop_log_a_sd",
                     "prior_pars_pop_log_b_mean", "prior_pars_pop_log_b_sd",
                     "prior_pars_pop_log_c_mean", "prior_pars_pop_log_c_sd",
                     "prior_pars_global_error_sigma"),
```
In `hmde_run_model()`, add two `rstan::sampling(stanmodels$zeide_*)` cases.

### `R/stanmodels.R`
Add `"zeide_multi_ind"` and `"zeide_single_ind"` to the `stanmodels` character vector (alphabetical order) and add the two corresponding `Rcpp::loadModule("stan_fit4zeide_*_mod", what = TRUE)` lines.

## Critical Files (reference)
- [R/hmde_model_names.R](R/hmde_model_names.R)
- [R/hmde_model.R](R/hmde_model.R)
- [R/hmde_model_des.R](R/hmde_model_des.R)
- [R/hmde_model_pars.R](R/hmde_model_pars.R)
- [R/hmde_run.R](R/hmde_run.R)
- [R/stanmodels.R](R/stanmodels.R)
- [inst/stan/canham_single_ind.stan](inst/stan/canham_single_ind.stan) (template)
- [inst/stan/canham_multi_ind.stan](inst/stan/canham_multi_ind.stan) (template)
- [tests/testthat/test-hmde_models_canham.R](tests/testthat/test-hmde_models_canham.R) (test template)

## Verification

1. `devtools::document()` — regenerate NAMESPACE, check no errors
2. `devtools::install()` — compiles Stan models (catches Stan syntax errors)
3. `devtools::test(filter = "zeide")` — runs the new test file
4. Quick smoke test in R console:
   ```r
   library(hmde)
   hmde_data_template("zeide_single_ind", obs_data = Trout_Size_Data[1:4,]) |>
     hmde_run(chains = 1, iter = 500, verbose = FALSE)
   ```
5. Verify `hmde_model_names()` returns both new model names
