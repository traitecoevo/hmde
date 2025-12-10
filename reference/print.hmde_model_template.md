# Print function for hmde_model_template object

Print function for hmde_model_template object

## Usage

``` r
# S3 method for class 'hmde_model_template'
print(x, ...)
```

## Arguments

- x:

  hmde_model_template output from hmde_model

- ...:

  other parameters used for print

## Examples

``` r
# basic usage of print
hmde_model("constant_single_ind") |> print()
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
