# hmde_data_template class An S4 class to prepare input data for a hmde model.

hmde_data_template class An S4 class to prepare input data for a hmde
model.

model_name getter Getter for model_name in hmde_data_template object

model_name setter Setter for model_name in hmde_data_template object

model_level getter Getter for model_level in hmde_data_template object

model_level setter Setter for model_level in hmde_data_template object

obs_data getter Getter for obs_data in hmde_data_template object

obs_data setter Setter for obs_data in hmde_data_template object

prior_pars getter Getter for prior_pars in hmde_data_template object

prior_pars setter Setter for prior_pars in hmde_data_template object

par_names getter Getter for par_names in hmde_data_template object

par_names setter Setter for par_names in hmde_data_template object.

Constructor Constructor function for hmde_data_template class, with data
assignment ready for model fitting.

Show function for hmde_data_template object

Print function for hmde_data_template object

Summary function for hmde_data_template object

Plot function for hmde_data_template object

## Usage

``` r
# S4 method for class 'hmde_data_template'
model_name(x)

# S4 method for class 'hmde_data_template'
model_name(x) <- value

# S4 method for class 'hmde_data_template'
model_level(x)

# S4 method for class 'hmde_data_template'
model_level(x) <- value

# S4 method for class 'hmde_data_template'
obs_data(x)

# S4 method for class 'hmde_data_template'
obs_data(x) <- value

# S4 method for class 'hmde_data_template'
prior_pars(x)

# S4 method for class 'hmde_data_template'
prior_pars(x) <- value

# S4 method for class 'hmde_data_template'
par_names(x)

# S4 method for class 'hmde_data_template'
par_names(x) <- value

hmde_data_template(
  model_name,
  obs_data = NULL,
  prior_pars = NULL,
  model_level = NA_character_,
  ...
)

# S4 method for class 'hmde_data_template'
show(object)

# S4 method for class 'hmde_data_template'
print(x)

# S4 method for class 'hmde_data_template'
summary(object)

# S4 method for class 'hmde_data_template,ANY'
plot(x)
```

## Arguments

- x:

  hmde_data_template class object

- value:

  list

- model_name:

  character string name of a hmde model

- obs_data:

  list or tibble containing observational data vectors

- prior_pars:

  list containing prior parameters

- model_level:

  character string specifying whether single or multiple inds

- ...:

  data-masking name-value pairs allowing specific input of elements

- object:

  hmde_data_template class object

## Value

hmde_data_template class object

## Slots

- `model_name`:

  character string

- `model_level`:

  character string

- `obs_data`:

  list of input data

- `prior_pars`:

  list of prior parameters

- `par_names`:

  list of model paramter names at each level

## Examples

``` r
# basic usage of hmde_data_template
hmde_data_template("constant_single_ind")
#> hmde_data_template
#>   Model name: constant_single_ind
#>   Model level:  single_ind
#>   Input data names:  n_obs, y_obs, obs_index, time
#>   Prior names:  prior_pars_ind_beta, prior_pars_global_error_sigma

# basic usage of hmde_data_template with data input
hmde_data_template("constant_single_ind",
  obs_data = list(
    n_obs = 3,
    y_obs = c(1,1,1),
    obs_index = 1:3,
    time = 0:2
  )
)
#> hmde_data_template
#>   Model name: constant_single_ind
#>   Model level:  single_ind
#>   Input data names:  n_obs, y_obs, obs_index, time
#>   Prior names:  prior_pars_ind_beta, prior_pars_global_error_sigma

#basic usage of hmde_data_template with prior input
hmde_data_template("constant_single_ind",
  prior_pars = list(
    prior_pars_ind_beta = c(1,2),
    prior_pars_global_error_sigma = c(1,3)
  )
)
#> hmde_data_template
#>   Model name: constant_single_ind
#>   Model level:  single_ind
#>   Input data names:  n_obs, y_obs, obs_index, time
#>   Prior names:  prior_pars_ind_beta, prior_pars_global_error_sigma
```
