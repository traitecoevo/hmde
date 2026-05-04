# Garcinia recondita model estimates - Barro Colorado Island data

Estimates object of class hmde_estimates for Canham fit. Contains
estimated sizes, individual growth parameters, and population-level
hyper-parameters for Garcinia recondita fit with a Canham growth
function hierarchical model. The data used to fit the model is the
Tree_Size_Data object.

## Usage

``` r
Tree_Size_Ests
```

## Format

### `Tree_Size_Ests`

A hmde_estimates object:

- model_name:

  A character string giving the model name - Canham with multiple
  individuals.

- model_level:

  A character string stating that the model is fit to multiple
  individuals.

- method:

  A character string stating that the model was fit with MCMC sampling.

- runtime:

  A matrix giving the runtime of each chain.

- fit_summary:

  A character string summarising the model fitting.

- measurement_ests:

  A tibble with 5 columns that gives information on size observations
  and estimates.

- individual_ests:

  A tibble with 13 columns that gives posterior estimates for individual
  growth parameters.

- population_ests:

  A tibble with 5 columns that gives posterior estimates for
  population-level hyper-parameters.

- error_ests:

  A tibble with 5 columns that gives posterior estimates of the error
  parameter.

- prior_pars:

  A list contianing details of the prior parameters used for the model
  fit.

- par_names:

  A list containing the names of estimated parameters at each level of
  the hierarchical model.
