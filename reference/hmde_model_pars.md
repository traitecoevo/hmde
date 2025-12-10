# Show parameter list for hmde supported model

Show parameter list for hmde supported model

## Usage

``` r
hmde_model_pars(model = NULL)
```

## Arguments

- model:

  model name character string

## Value

named list that matches Stan model parameters

## Examples

``` r
# basic usage of hmde_model_pars
hmde_model_pars("constant_single_ind")
#> $measurement_pars_names
#> [1] "y_hat"
#> 
#> $individual_pars_names
#> [1] "ind_beta"
#> 
#> $error_pars_names
#> [1] "global_error_sigma"
#> 
#> $model
#> [1] "constant_single_ind"
#> 
```
