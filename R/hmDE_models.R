#' Select data configuration template for hmDE supported model
#'
#' @param model model name character string
#'
#' @return named list that matches Stan model parameters
#' @export

hmDE_model <- function(model=NULL){

  #TODO: Need a mechanism to check model requested in one that is supported by hmDE

  output <- switch(model,
                   constant_single_ind = hmDE_const_single_ind(),
                   constant_multi_ind = hmDE_const_multi_ind(),
                   canham_single_ind = hmDE_canham_single_ind(),
                   canham_multi_ind = hmDE_canham_multi_ind(),
                   power_single_ind = hmDE_power_single_ind(),
                   power_multi_ind = hmDE_power_multi_ind(),
                   vb_single_ind = hmDE_vb_single_ind(),
                   vb_multi_ind = hmDE_vb_multi_ind(),
                   linear_single_ind = hmDE_linear_single_ind())

  class(output) <- "hmDE_object"

  return(output)
}

#' Data configuration template for linear model
#' @keywords internal
#' @noRd

hmDE_lm <- function(){
  list(X = NULL,
       Y = NULL,
       N = NULL,
       model = "linear")
}

#' Data configuration template for constant growth single individual model
#' @keywords internal
#' @noRd

hmDE_const_single_ind <- function(){
  list(n_obs = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       y_0_obs = NULL,
       model = "constant_single_ind")
}

#' Data configuration template for constant growth single species model
#' @keywords internal
#' @noRd

hmDE_const_multi_ind <- function(){
  list(n_obs = NULL,
       n_ind = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       ind_id = NULL,
       y_0_obs = NULL,
       model = "constant_multi_ind")
}

#' Data configuration template for Canham growth single individual model
#' @keywords internal
#' @noRd

hmDE_canham_single_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       y_0_obs = NULL,
       model = "canham_single_ind")
}

#' Data configuration template for Canham growth single species model
#' @keywords internal
#' @noRd

hmDE_canham_multi_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       n_ind = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       ind_id = NULL,
       y_0_obs = NULL,
       model = "canham_multi_ind")
}

#' Data configuration template for power law growth single individual model
#' @keywords internal
#' @noRd

hmDE_power_single_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       y_0_obs = NULL,
       y_bar = NULL,
       model = "power_single_ind")
}

#' Data configuration template for power law growth single species model
#' @keywords internal
#' @noRd

hmDE_power_multi_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       n_ind = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       ind_id = NULL,
       y_0_obs = NULL,
       y_bar = NULL,
       model = "power_multi_ind")
}

#' Data configuration template for von Bertalanffy growth single individual model
#' @keywords internal
#' @noRd

hmDE_vb_single_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       y_0_obs = NULL,
       y_bar = NULL,
       model = "vb_single_ind")
}

#' Data configuration template for von Bertalanffy growth single species model
#' @keywords internal
#' @noRd

hmDE_vb_multi_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       n_ind = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       ind_id = NULL,
       y_0_obs = NULL,
       y_bar = NULL,
       model = "vb_multi_ind")
}

#' Data configuration template for linear growth single individual model
#' @keywords internal
#' @noRd
#'
hmDE_linear_single_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       y_0_obs = NULL,
       y_bar = NULL,
       model = "linear_single_ind")
}
