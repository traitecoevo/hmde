#' Function to select DE given model name
#' @param model character string model name
#'
#' @return DE function corresponding to specific model
#' @export

hmde_model_des <- function(model = NULL){
  if(!model %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  output <- switch(
    model,
    constant_single_ind = hmde_const_de,
    constant_multi_ind = hmde_const_de,
    canham_single_ind = hmde_canham_de,
    canham_multi_ind = hmde_canham_de,
    vb_single_ind = hmde_vb_de,
    vb_multi_ind = hmde_vb_de,
    linear_single_ind = hmde_linear_de
  )

  return(output)
}

#' Differential equation for constant growth single and multi- individual models
#' @param y input real
#' @param pars list of parameter beta
#'
#' @return value of differential equation at y
#' @export

hmde_const_de <- function(y = NULL, pars = NULL){
  return(
    pars[[1]]
    )
}


#' Differential equation for Canham growth single and multi- individual models
#' @param y input real
#' @param pars list of parametera g_max, S_max, k
#'
#' @return value of differential equation at y
#' @export

hmde_canham_de <- function(y = NULL, pars = NULL){
  return(
    pars[[1]] * exp(-0.5 *
                      ((log(y / pars[[2]]) / pars[[3]]) ^ 2)
                    )
    )
}

#' Differential equation for von Bertalanffy growth single and multi- individual models
#' @param y input real
#' @param pars list of parameteters Y_max, growth_rate
#'
#' @return value of differential equation at y
#' @export

hmde_vb_de <- function(y = NULL, pars = NULL){
  return(
    pars[[2]] * (pars[[1]] - y)
    )
}

#' Differential equation for linear growth single individual model
#' @param y input real
#' @param pars list of parameters beta_0, beta_1
#'
#' @return value of differential equation at y
#' @export

hmde_linear_de <- function(y = NULL, pars = NULL){
  return(
    pars[[1]] - pars[[2]] * y
    )
}
