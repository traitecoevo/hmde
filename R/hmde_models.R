#' Select data configuration template for hmde supported model
#'
#' @param model model name character string
#'
#' @return named list that matches Stan model parameters
#' @export

hmde_model <- function(model=NULL){

  if(!model %in% hmde_model_name()){
    stop("Model name not recognised. Run hmde_model_name() to see available models.")
  }

  output <- switch(model,
                   constant_single_ind = hmde_const_single_ind(),
                   constant_multi_ind = hmde_const_multi_ind(),
                   canham_single_ind = hmde_canham_single_ind(),
                   canham_multi_ind = hmde_canham_multi_ind(),
                   power_single_ind = hmde_power_single_ind(),
                   power_multi_ind = hmde_power_multi_ind(),
                   vb_single_ind = hmde_vb_single_ind(),
                   vb_multi_ind = hmde_vb_multi_ind(),
                   linear_single_ind = hmde_linear_single_ind())

  class(output) <- "hmde_object"

  return(output)
}

#' Data configuration template for constant growth single individual model
#' @keywords internal
#' @noRd

hmde_const_single_ind <- function(){
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

hmde_const_multi_ind <- function(){
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

hmde_canham_single_ind <- function(){
  list(n_obs = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       y_0_obs = NULL,
       model = "canham_single_ind")
}

#' Data configuration template for Canham growth single species model
#' @keywords internal
#' @noRd

hmde_canham_multi_ind <- function(){
  list(n_obs = NULL,
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

hmde_power_single_ind <- function(){
  list(n_obs = NULL,
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

hmde_power_multi_ind <- function(){
  list(n_obs = NULL,
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

hmde_vb_single_ind <- function(){
  list(n_obs = NULL,
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

hmde_vb_multi_ind <- function(){
  list(n_obs = NULL,
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
hmde_linear_single_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       y_0_obs = NULL,
       y_bar = NULL,
       model = "linear_single_ind")
}
