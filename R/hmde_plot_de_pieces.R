#' Plot pieces of chosen differential equation model for each individual.
#' Structured to take the individual data tibble that is built by the
#' hmde_extract_estimates function using the ind_par_name_mean estimates.
#' Function piece will go from the first fitted size to the last.
#' Accepted ggplot arguments will change the axis labels, title, line colour, alpha
#'
#' @param estimate_list list output from hmde_extract_estimates
#' @param xlab character string for replacement x axis label
#' @param ylab character string for replacement y axis label
#' @param title character string for replacement plot title
#' @param colour character string for replacement line colour
#' @param alpha real number for replacement alpha value
#'
#' @return ggplot object
#'
#' @examples
#' # basic usage of hmde_plot_de_pieces
#' hmde_plot_de_pieces(estimate_list = Tree_Size_Ests)
#'
#' @export
#' @import ggplot2
#' @import dplyr

hmde_plot_de_pieces <- function(estimate_list = NULL,
                                xlab = "Y(t)",
                                ylab = "f",
                                title = NULL,
                                colour = "#006600",
                                alpha = 0.4){
  model <- estimate_list$model
  individual_data <- estimate_list$individual_data
  measurement_data <- estimate_list$measurement_data

  #Check for model
  if(!model %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  if(is.null(model)){
    stop("Model not provided.")
  }

  if(is.null(individual_data)){
    stop("Individual parameter data not provided.")
  }

  if(is.null(measurement_data)){
    stop("Measurement data not provided.")
  }

  #Get individual parameter estimates
  model_par_names <- hmde_model_pars(model)
  pars_data <- tibble(ind_id = individual_data$ind_id)
  for(i in model_par_names$individual_pars_names){
    pars_data[[i]] <- individual_data[[paste0(i, "_mean")]]
  }

  #Extract initial and final sizes for each individual
  initial_and_final_vals <- measurement_data %>%
    group_by(ind_id) %>%
    arrange(obs_index) %>%
    mutate(y_0 = first(y_hat),
           y_final = last(y_hat)) %>%
    ungroup() %>%
    select(ind_id, y_0, y_final) %>%
    distinct()

  #Generate plot
  plot <- hmde_ggplot_de_pieces(pars_data = pars_data,
                                y_0 = initial_and_final_vals$y_0,
                                y_final = initial_and_final_vals$y_final,
                                DE_function = hmde_model_des(model),
                                xlab = xlab,
                                ylab = ylab,
                                title = title,
                                colour = colour,
                                alpha = alpha)

  return(plot)
}

#' Produce plot for plot_de_pieces
#' @keywords internal
#' @noRd
hmde_ggplot_de_pieces <- function(pars_data,
                                  y_0,
                                  y_final,
                                  DE_function,
                                  xlab,
                                  ylab,
                                  title,
                                  colour,
                                  alpha){
  plot <- ggplot() +
    xlim(min(y_0), max(y_final)) +
    labs(x = xlab, y = ylab, title = title) +
    theme_classic()# +
    #theme(axis.text=element_text(size=16),
    #      axis.title=element_text(size=18,face="bold"))

  for(i in 1:nrow(pars_data)){
    args_list <- list(pars=pars_data[i,-1]) #Remove ind_id
    plot <- plot +
      geom_function(fun=DE_function, args=args_list,
                    colour=colour, linewidth=1, alpha = alpha,
                    xlim=c(y_0[i], y_final[i]))
  }

  return(plot)
}

#' Set variable names to be regarded as globally defined
#' @noRd
globalVariables(c("obs_index", "y_0", "y_final"),
                "hmde",
                add = TRUE)
