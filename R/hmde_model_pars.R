#' Show parameter list for hmde supported model
#'
#' @param model model name character string
#'
#' @return named list that matches Stan model parameters
#' @export

hmde_model_pars <- function(model=NULL){

  if(!model %in% hmde_model_name()){
    print("Model not available.")
    return(-1)
  }

  output <- switch(model,
                   constant_single_ind = hmde_const_single_ind_pars(),
                   constant_multi_ind = hmde_const_multi_ind_pars(),
                   canham_single_ind = hmde_canham_single_ind_pars(),
                   canham_multi_ind = hmde_canham_multi_ind_pars(),
                   power_single_ind = hmde_power_single_ind_pars(),
                   power_multi_ind = hmde_power_multi_ind_pars(),
                   vb_single_ind = hmde_vb_single_ind_pars(),
                   vb_multi_ind = hmde_vb_multi_ind_pars(),
                   linear_single_ind = hmde_linear_single_ind_pars())

  return(output)
}

#' Data configuration template for constant growth single individual model
#' @keywords internal
#' @noRd

hmde_const_single_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat", "Delta_hat"),
       individual_pars_names = c("ind_beta"),
       error_pars_names = c("global_error_sigma"),
       model = "constant_single_ind")
}

#' Data configuration template for constant growth single pop model
#' @keywords internal
#' @noRd

hmde_const_multi_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat", "Delta_hat"),
       individual_pars_names = c("ind_beta"),
       population_pars_names = c("pop_beta_mu", "pop_beta_sigma"),
       error_pars_names = c("global_error_sigma"),
       model = "constant_single_ind")
}

#' Data configuration template for Canham growth single individual model
#' @keywords internal
#' @noRd

hmde_canham_single_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat", "Delta_hat"),
       individual_pars_names = c("ind_max_growth", "ind_diameter_at_max_growth", "ind_k"),
       error_pars_names = c("global_error_sigma"),
       model = "canham_single_ind")
}

#' Data configuration template for Canham growth single pop model
#' @keywords internal
#' @noRd

hmde_canham_multi_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat", "Delta_hat"),
       individual_pars_names = c("ind_max_growth", "ind_diameter_at_max_growth", "ind_k"),
       population_pars_names = c("pop_max_growth_mu", "pop_max_growth_sigma",
                      "pop_diameter_at_max_growth_mu", "pop_diameter_at_max_growth_sigma",
                      "pop_k_mu", "pop_k_sigma"),
       error_pars_names = c("global_error_sigma"),
       model = "canham_multi_ind")
}

#' Data configuration template for power law growth single individual model
#' @keywords internal
#' @noRd

hmde_power_single_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat", "Delta_hat"),
       individual_pars_names = c("ind_coeff", "ind_power"),
       error_pars_names = c("global_error_sigma"),
       model = "power_single_ind")
}

#' Data configuration template for power law growth single pop model
#' @keywords internal
#' @noRd

hmde_power_multi_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat", "Delta_hat"),
       individual_pars_names = c("ind_coeff", "ind_power"),
       population_pars_names = c("pop_coeff_mu", "pop_coeff_sigma",
                      "pop_power_mu", "pop_power_sigma"),
       error_pars_names = c("global_error_sigma"),
       model = "power_multi_ind")
}

#' Data configuration template for von Bertalanffy growth single individual model
#' @keywords internal
#' @noRd

hmde_vb_single_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat", "Delta_hat"),
       individual_pars_names = c("ind_max_size", "ind_growth_rate"),
       error_pars_names = c("global_error_sigma"),
       model = "vb_single_ind")
}

#' Data configuration template for von Bertalanffy growth single pop model
#' @keywords internal
#' @noRd

hmde_vb_multi_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat", "Delta_hat"),
       individual_pars_names = c("ind_max_size", "ind_growth_rate"),
       population_pars_names = c("pop_max_size_mu", "pop_max_size_sigma",
                      "pop_growth_rate_mu", "pop_growth_rate_sigma"),
       error_pars_names = c("global_error_sigma"),
       model = "vb_multi_ind")
}

#' Data configuration template for linear growth single individual model
#' @keywords internal
#' @noRd
#'
hmde_linear_single_ind_pars <- function(){
  list(measurement_pars_names = c("y_hat", "Delta_hat"),
       individual_pars_names = c("ind_beta_0", "ind_beta_1"),
       error_pars_names = c("global_error_sigma"),
       model = "linear_single_ind")
}
