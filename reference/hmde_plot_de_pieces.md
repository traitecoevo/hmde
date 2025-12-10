# Plot pieces of chosen differential equation model for each individual. Structured to take the individual data tibble that is built by the hmde_extract_estimates function using the ind_par_name_mean estimates. Function piece will go from the first fitted size to the last. Accepted ggplot arguments will change the axis labels, title, line colour, alpha

Plot pieces of chosen differential equation model for each individual.
Structured to take the individual data tibble that is built by the
hmde_extract_estimates function using the ind_par_name_mean estimates.
Function piece will go from the first fitted size to the last. Accepted
ggplot arguments will change the axis labels, title, line colour, alpha

## Usage

``` r
hmde_plot_de_pieces(
  estimate_list = NULL,
  xlab = "Y(t)",
  ylab = "f",
  title = NULL,
  colour = "#006600",
  alpha = 0.4
)
```

## Arguments

- estimate_list:

  list output from hmde_extract_estimates

- xlab:

  character string for replacement x axis label

- ylab:

  character string for replacement y axis label

- title:

  character string for replacement plot title

- colour:

  character string for replacement line colour

- alpha:

  real number for replacement alpha value

## Value

ggplot object

## Examples

``` r
# basic usage of hmde_plot_de_pieces
hmde_plot_de_pieces(estimate_list = Tree_Size_Ests)

```
