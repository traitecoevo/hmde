# Garcinia recondita model estimates - Barro Colorado Island data

Estimated sizes, individual growth parameters, and population-level
hyper-parameters for Garcinia recondita fit with a Canham growth
function hierarchical model. The data used to fit the model is the
Tree_Size_Data object.

## Usage

``` r
Tree_Size_Ests
```

## Format

### `Tree_Size_Ests`

A list with 5 elements:

- model:

  A character string giving the model name - Canham with multiple
  individuals.

- measurement_data:

  A tibble with 5 columns that gives information on size observations
  and estimates.

- individual_data:

  A tibble with 13 columns that gives posterior estimates for individual
  growth parameters.

- error_data:

  A tibble with 5 columns that gives posterior estimates of the error
  parameter.

- population_data:

  A tibble with 5 columns that gives posterior estimates for
  population-level hyper-parameters.
