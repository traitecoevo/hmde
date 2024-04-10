#' Select data configuration template for rmot supported model
#'
#' @param model model name character string
#'
#' @return named list that matches Stan model parameters
#' @export
#'
#' @examples
#' rmot_model("linear")

rmot_model <- function(model=NULL){

  #TODO: Need a mechanism to check model requested in one that is supported by rmot

  output <- switch(model,
                   linear = rmot_lm(),
                   constant_single_ind = rmot_const_single_ind(),
                   constant_multi_ind = rmot_const_multi_ind(),
                   canham_single_ind = rmot_canham_single_ind(),
                   canham_multi_ind = rmot_canham_multi_ind(),
                   power_single_ind = rmot_power_single_ind(),
                   power_multi_ind = rmot_power_multi_ind())

  class(output) <- "rmot_object"

  return(output)
}

#' Data configuration template for linear model
#' @keywords internal
#' @noRd

rmot_lm <- function(){
  list(X = NULL,
       Y = NULL,
       N = NULL,
       model = "linear")
}

#' Data configuration template for constant growth single individual model
#' @keywords internal
#' @noRd

rmot_const_single_ind <- function(){
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

rmot_const_multi_ind <- function(){
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

rmot_canham_single_ind <- function(){
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

rmot_canham_multi_ind <- function(){
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

rmot_power_single_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       y_bar = NULL,
       y_0_obs = NULL,
       model = "power_single_ind")
}

#' Data configuration template for power law growth single species model
#' @keywords internal
#' @noRd

rmot_power_multi_ind <- function(){
  list(step_size = NULL,
       n_obs = NULL,
       n_ind = NULL,
       y_obs = NULL,
       obs_index = NULL,
       time = NULL,
       ind_id = NULL,
       y_bar = NULL,
       y_0_obs = NULL,
       model = "power_multi_ind")
}
