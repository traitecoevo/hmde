#' The 'hmde' package.
#'
#' @description A package to implement a selection of hierarchical Bayesian longitudinal models for inverse Bayesian problems.
#'
#' @docType package
#' @name hmde-package
#' @aliases hmde
#' @useDynLib hmde, .registration = TRUE
#' @import methods
#' @import dplyr
#' @import ggplot2
#' @import RcppParallel
#' @import rlang
#' @import rstan
#' @import rstantools
#' @import tibble
#' @import cowplot
#' @import knitr
#' @importFrom Rcpp loadModule
#' @importFrom purrr map
#' @importFrom purrr list_modify
#' @importFrom cowplot plot_grid
#' @importFrom rstan sampling
#' @importFrom rstan Rhat
#' @importFrom rstan get_elapsed_time
#' @importFrom knitr kable
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats setNames
#' @importFrom cowplot plot_grid
#' @importFrom utils head
#'
#' @references
#' Stan Development Team (NA). RStan: the R interface to Stan. R package version 2.26.23. https://mc-stan.org
#'
"_PACKAGE"
NULL
