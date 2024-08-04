#' Differential equation for constant growth single and multi- individual models
#' @param y
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
#' @param y
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

#' Differential equation for power law growth single and multi- individual models
#' @param y
#' @param pars list of parameters coefficient, power
#'
#' @return value of differential equation at y
#' @export

hmde_power_de <- function(y = NULL, pars = NULL){
  return(
    pars[[1]] * (y ^ -pars[[2]])
    )
}


#' Differential equation for von Bertalanffy growth single and multi- individual models
#' @param y
#' @param pars list of parameters beta, Y_max
#'
#' @return value of differential equation at y
#' @export

hmde_vb_de <- function(y = NULL, pars = NULL){
  return(
    pars[[2]] * (pars[[1]] - y)
    )
}

#' Differential equation for linear growth single individual model
#' @param y
#' @param pars list of parameters beta_0, beta_1
#'
#' @return value of differential equation at y
#' @export

hmde_linear_de <- function(y = NULL, pars = NULL){
  return(
    pars[[1]] + pars[[2]] * y
    )
}
