#' Print function for hmde_model_template object
#'
#' @param model_template hmde_model_template output from hmde_model
#'
#' @examples
#' # basic usage of print.hmde_model_template
#' hmde_model("constant_single_ind") |> print.hmde_model_template()
#'
#' @export

print.hmde_model_template <- function(model_template = NULL){
  print(model_template)
}


#' Summary function for hmde_model_template object
#'
#' @param model_template hmde_model_template output from hmde_model
#'
#' @examples
#' # basic usage of summary.hmde_model_template
#' hmde_model("constant_single_ind") |> print.hmde_model_template()
#'
#' @export

summary.hmde_model_template <- function(model_template = NULL){
  print(model_template)
}


#' Plot function for hmde_model_template object
#'
#' @param model_template hmde_model_template output from hmde_model
#'
#' @examples
#' # basic usage of plot.hmde_model_template
#' hmde_model("constant_single_ind") |> plot.hmde_model_template()
#'
#' @export

plot.hmde_model_template <- function(model_template = NULL){
  model_pars <- hmde_model_pars(model_template$model)

  #Get parameter estimates from mean of prior distribution
  if(grepl("multi", model_template$model, fixed = TRUE)){ #If multi-ind model

  } else { #Single individual model

  }

  #Check that size does not exceed max for VB models
  if(grepl("vb", model_template$model, fixed = TRUE)){
    y_final <- pars$max_size
  } else {
    y_final <- 50
  }


  #Plot ODE using parameter estimates
  plot <- hmde_ggplot_de_pieces(pars_data = pars,
                        y_0 = 1,
                        y_final = y_final,
                        DE_function = hmde_model_des(model_template$model),
                        xlab = "Size",
                        ylab = "Growth rate",
                        title = paste0("Example model for ", model_template$model),
                        colour = "#006600",
                        alpha = 0.4)

  return(plot)
}

#' Print function for hmde_fit object
#'
#' @param fit hmde_fit output from hmde_run
#'
#' @examples
#' # basic usage of hmde_assign_data
#' hmde_model("constant_single_ind") |> hmde_assign_data(Trout_Size_Data)
#'
#' @export

print.hmde_fit <- function(fit = NULL){

}


#' Summary function for hmde_fit object
#'
#' @param fit hmde_fit output from hmde_run
#'
#' @examples
#' # basic usage of hmde_assign_data
#' hmde_model("constant_single_ind") |> hmde_assign_data(Trout_Size_Data)
#'
#' @export

summary.hmde_fit <- function(fit = NULL){

}


#' Plot function for hmde_fit object
#'
#' @param fit hmde_fit output from hmde_run
#'
#' @examples
#' # basic usage of hmde_assign_data
#' hmde_model("constant_single_ind") |> hmde_assign_data(Trout_Size_Data)
#'
#' @export

plot.hmde_fit <- function(fit = NULL){

}
