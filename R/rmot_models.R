#' Select data configuration template for rmot supported model
#'
#' @param model model name character string
#'
#' @return named list that matches Stan model parameters
#' @details
#' - X is the predictor
#' - Y is the response
#' @export
#'
#' @examples
#' rmot_model("linear")

rmot_model <- function(model=NULL){

  #TODO: Need a mechanism to check model requested in one that is supported by rmot

  output <- switch(model,
                   linear = rmot_lm(),
                   constant_single = rmot_cgs())

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

#' Data configuration template for constant growth single species model
#' @keywords internal
#' @noRd

rmot_cgs <- function(){
  list(N_obs = NULL,
       N_ind = NULL,
       S_obs = NULL,
       census = NULL,
       census_interval = NULL,
       id_factor = NULL,
       S_0_obs = NULL,
       model = "constant_single")
}



