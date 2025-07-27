#' Print function for hmde_model_template object
#'
#' @param x hmde_model_template output from hmde_model
#'
#' @examples
#' # basic usage of print.hmde_model_template
#' hmde_model("constant_single_ind") |> print.hmde_model_template()
#'
#' @export

print.hmde_model_template <- function(x, ...){
  print(paste0("Model: ", x$model))
  print("Input data template:")
  for(i in 1:length(x)){
    print(x[i])
  }
}


#' Summary function for hmde_model_template object
#'
#' @param object hmde_model_template output from hmde_model
#'
#' @examples
#' # basic usage of summary.hmde_model_template
#' hmde_model("constant_single_ind") |> print.hmde_model_template()
#'
#' @export

summary.hmde_model_template <- function(object, ...){
  print(paste0("Model: ", object$model))
  print("Input data template:")
  for(i in 1:length(object)){
    print(object[i])
  }
}


#' Plot function for hmde_model_template object
#'
#' @param x hmde_model_template output from hmde_model
#'
#' @examples
#' # basic usage of plot.hmde_model_template
#' hmde_model("constant_single_ind") |> plot.hmde_model_template()
#'
#' @export

plot.hmde_model_template <- function(x, ...){
  model_pars_names <- hmde_model_pars(model_template$model)$individual_pars_names

  temp <- c()
  #Get parameter estimates from mean of prior distribution
  for(i in 1:length(model_pars_names)){
    #Get prior name
    if(grepl("multi", model_template$model, fixed = TRUE)){ #If multi-ind model
      prior_name <- paste0("prior_pars_pop_log_",
                           gsub("ind_", "", model_pars_names[i]), "_mean")
    } else { #Single individual model
      prior_name <- paste0("prior_pars_", model_pars_names[i])
    }

    #Cover von Bertalanffy prior centering at provided data max
    if(grepl("vb", model_template$model, fixed = TRUE) &&
       grepl("max_size", model_pars_names[i], fixed = TRUE)){
      temp[i+1] <- 10
    } else {
      temp[i+1] <- exp(model_template[[prior_name]][1])
    }
  }
  plot_pars <- rbind(temp)

  #Get final size for plot
  if(grepl("vb", model_template$model, fixed = TRUE)){
    y_final <- plot_pars[1,2]
  } else {
    y_final <- 20
  }

  #Plot ODE using parameter estimates
  plot <- hmde_ggplot_de_pieces(pars_data = plot_pars,
                        y_0 = 0,
                        y_final = y_final,
                        DE_function = hmde_model_des(model_template$model),
                        xlab = "Size",
                        ylab = "Growth rate",
                        title = paste0("Example ODE for ", model_template$model),
                        colour = "#006600",
                        alpha = 0.4)

  return(plot)
}

#' Print function for hmde_fit object
#'
#' @param x hmde_fit output from hmde_run
#'
#' @examples
#' # basic usage of print.hmde_fit
#' hmde_model("constant_single_ind") |>
#'   hmde_assign_data(Trout_Size_Data)|>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   print.hmde_fit()
#'
#' @export

print.hmde_fit <- function(x, ...){
  #Get model name info
  model <- x@model_name
  par_names <- hmde_model_pars(model)

  #Get sample info
  samples <- rstan::extract(x, permuted = TRUE, inc_warmup = FALSE)
  sample_par_names <- names(samples)

  if(grepl("multi", model, fixed=TRUE)){ #Multi-species model has population level
    pars_out_table <- hmde_extract_individual_par_ests(samples,
                                                  par_names$population_pars_names)
  } else { #Individual level is highest
    pars_out_table <- hmde_extract_individual_par_ests(samples,
                                     par_names$individual_pars_names,
                                     n_ind = 1)
  }
}


#' Summary function for hmde_fit object
#'
#' @param object hmde_fit output from hmde_run
#'
#' @examples
#' # basic usage of summary.hmde_fit
#' hmde_model("constant_single_ind") |>
#'   hmde_assign_data(Trout_Size_Data)|>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   summary.hmde_fit()
#'
#' @export

summary.hmde_fit <- function(object, ...){

}


#' Plot function for hmde_fit object
#'
#' @param x hmde_fit output from hmde_run
#'
#' @examples
#' # basic usage of plot.hmde_fit
#' hmde_model("constant_single_ind") |>
#'   hmde_assign_data(Trout_Size_Data)|>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   plot.hmde_fit()
#'
#' @export

plot.hmde_fit <- function(x, ...){

}
