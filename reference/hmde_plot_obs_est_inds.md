# Plot estimated and observed values over time for a chosen number of individuals based on posterior estimates. Structured to take in the measurement_data tibble constructed by the hmde_extract_estimates function.

Plot estimated and observed values over time for a chosen number of
individuals based on posterior estimates. Structured to take in the
measurement_data tibble constructed by the hmde_extract_estimates
function.

## Usage

``` r
hmde_plot_obs_est_inds(
  estimate_list = NULL,
  measurement_data = NULL,
  ind_id_vec = NULL,
  n_ind_to_plot = NULL,
  xlab = "Time",
  ylab = "Y(t)",
  title = NULL
)
```

## Arguments

- estimate_list:

  list output of hmde_extract_estimates

- measurement_data:

  tibble with estimated measurements

- ind_id_vec:

  vector with list of ind_id values

- n_ind_to_plot:

  integer giving number of individuals to plot if not specified

- xlab:

  character string for replacement x axis label

- ylab:

  character string for replacement y axis label

- title:

  character string for replacement plot title

## Value

ggplot object

## Examples

``` r
# basic usage of hmde_plot_obs_est_inds
hmde_plot_obs_est_inds(estimate_list = Tree_Size_Ests,
                       n_ind_to_plot = 5)

```
