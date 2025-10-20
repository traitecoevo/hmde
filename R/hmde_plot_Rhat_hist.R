#' Plot histogram of R_hat values for hmde_fit object.
#'
#' @param fit hmde_fit object output from hmde_run
#'
#' @return ggplot object
#'
#' @examples
#' # basic usage of hmde_plot_Rhat_hist
#' hmde_model("constant_single_ind") |>
#'   hmde_assign_data(Trout_Size_Data)|>
#'   hmde_run(chains = 2, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   hmde_plot_Rhat_hist()
#'
#' @export
#' @import ggplot2
#' @importFrom rstan Rhat

hmde_plot_Rhat_hist <- function(fit){
  #hmde_extract_Rhat excludes the NaNs associated with constant generated values
  rhat_vals <- tibble(
    rhat = as.numeric(hmde_extract_Rhat(fit)),
    name = names(hmde_extract_Rhat(fit))
  )

  n <- nrow(rhat_vals)
  nan_n <- sum(is.nan(rhat_vals$rhat))

  plot <- ggplot(rhat_vals, aes(x = rhat)) +
    geom_histogram() +
    labs(title = paste0(nan_n, " R_hat values out of ", n)) +
    theme_classic()

  return(plot)
}
