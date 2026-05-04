# helper function for hmde_data_template that provides data structure for model, Also used to guide the user on the required structure for the model they want.

helper function for hmde_data_template that provides data structure for
model, Also used to guide the user on the required structure for the
model they want.

## Usage

``` r
hmde_model(model = NULL)
```

## Arguments

- model:

  model name character string

## Value

hmde_data_template class that suits Stan model input

## Examples

``` r
# basic usage of hmde_model
hmde_model("constant_single_ind")
#> hmde_data_template
#>   Model name: constant_single_ind
#>   Model level:  single_ind
#>   Input data names:  n_obs, y_obs, obs_index, time
#>   Prior names:  prior_pars_ind_beta, prior_pars_global_error_sigma
```
