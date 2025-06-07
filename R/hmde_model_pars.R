#' Show parameter list for hmde supported model
#'
#' @param model model name character string
#'
#' @examples
#' # basic usage of hmde_model_pars
#' hmde_model_pars("constant_single_ind")
#'
#' @return named list that matches Stan model parameters
#' @export

hmde_model_pars <- function(model=NULL){

  if(!model %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  output <- switch(model,
                   constant_single_ind = hmde_const_single_ind_pars(),
                   constant_multi_ind = hmde_const_multi_ind_pars(),
                   canham_single_ind = hmde_canham_single_ind_pars(),
                   canham_multi_ind = hmde_canham_multi_ind_pars(),
                   vb_single_ind = hmde_vb_single_ind_pars(),
                   vb_multi_ind = hmde_vb_multi_ind_pars(),
                   affine_single_ind = hmde_affine_single_ind_pars())

  return(output)
}

#' Parameter names for constant growth single individual model
#' @keywords internal
#' @noRd

hmde_const_single_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat"),
       individual_pars_names = c("ind_beta"),
       error_pars_names = c("global_error_sigma"),
       model = "constant_single_ind")
}

#' Parameter names for constant growth single pop model
#' @keywords internal
#' @noRd

hmde_const_multi_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat"),
       individual_pars_names = c("ind_beta"),
       population_pars_names = c("pop_log_beta_mu", "pop_log_beta_sigma"),
       error_pars_names = c("global_error_sigma"),
       model = "constant_multi_ind")
}

#' Parameter names for Canham growth single individual model
#' @keywords internal
#' @noRd

hmde_canham_single_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat"),
       individual_pars_names = c("ind_max_growth", "ind_size_at_max_growth", "ind_k"),
       error_pars_names = c("global_error_sigma"),
       model = "canham_single_ind")
}

#' Parameter names for Canham growth single pop model
#' @keywords internal
#' @noRd

hmde_canham_multi_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat"),
       individual_pars_names = c("ind_max_growth", "ind_size_at_max_growth", "ind_k"),
       population_pars_names = c("pop_log_max_growth_mean", "pop_log_max_growth_sd",
                      "pop_log_size_at_max_growth_mean", "pop_log_size_at_max_growth_sd",
                      "pop_log_k_mean", "pop_log_k_sd"),
       error_pars_names = c("global_error_sigma"),
       model = "canham_multi_ind")
}

#' Parameter names for von Bertalanffy growth single individual model
#' @keywords internal
#' @noRd

hmde_vb_single_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat"),
       individual_pars_names = c("ind_max_size", "ind_growth_rate"),
       error_pars_names = c("global_error_sigma"),
       model = "vb_single_ind")
}

#' Parameter names for von Bertalanffy growth single pop model
#' @keywords internal
#' @noRd

hmde_vb_multi_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat"),
       individual_pars_names = c("ind_max_size", "ind_growth_rate"),
       population_pars_names = c("pop_log_max_size_mean", "pop_log_max_size_sd",
                      "pop_log_growth_rate_mean", "pop_log_growth_rate_sd"),
       error_pars_names = c("global_error_sigma"),
       model = "vb_multi_ind")
}

#' Parameter names for affine growth single individual model
#' @keywords internal
#' @noRd
#'
hmde_affine_single_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat"),
       individual_pars_names = c("ind_beta_0", "ind_beta_1"),
       error_pars_names = c(NULL),
       model = "affine_single_ind")
}
