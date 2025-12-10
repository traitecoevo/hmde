# Select data configuration template for hmde supported model

Select data configuration template for hmde supported model

## Usage

``` r
hmde_model(model = NULL)
```

## Arguments

- model:

  model name character string

## Value

hmde_model_template class named list that matches Stan model parameters

## Examples

``` r
# basic usage of hmde_model
hmde_model("constant_single_ind")
#> [1] "Model: constant_single_ind"
#> [1] "Input data template:"
#> $n_obs
#> NULL
#> 
#> $y_obs
#> NULL
#> 
#> $obs_index
#> NULL
#> 
#> $time
#> NULL
#> 
#> $prior_pars_ind_beta
#> [1] 0 2
#> 
#> $prior_pars_global_error_sigma
#> [1] 0 2
#> 
#> $model
#> [1] "constant_single_ind"
#> 
```
