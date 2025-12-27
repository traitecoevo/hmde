# Function to select DE given model name

Function to select DE given model name

## Usage

``` r
hmde_model_des(model = NULL)
```

## Arguments

- model:

  character string model name

## Value

DE function corresponding to specific model

## Examples

``` r
# basic usage of hmde_model_des
hmde_model_des("constant_single_ind")
#> function (y = NULL, pars = NULL) 
#> {
#>     return(pars[[1]])
#> }
#> <bytecode: 0x5634539c3708>
#> <environment: namespace:hmde>
```
