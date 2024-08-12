#' Plot pieces of chosen differential equation model for each individual.
#' Function piece will go from the first fitted size to the last.
#' Accepted ggplot arguments will change the axis labels, title, line colour, alpha
#'
#' @param model model name character string
#' @param individual_data tibble with estimated DE parameters
#' @param measurement_data tibble with estimated measurements
#' @param xlab character string for replacement x axis label
#' @param ylab character string for replacement y axis label
#' @param title character string for replacement plot title
#' @param colour character string for replacement line colour
#' @param alpha real number for replacement alpha value
#'
#' @return ggplot object
#' @export
#' @import ggplot2
#' @import dplyr

hmde_plot_de_pieces <- function(model = NULL,
                                individual_data = NULL,
                                measurement_data = NULL,
                                xlab = "Y(t)",
                                ylab = "f",
                                title = NULL,
                                colour = "#006600",
                                alpha = 0.2){
  #Extract initial and final sizes for each individual
  initial_and_final_vals <- measurement_data %>%
    group_by(ind_id) %>%
    arrange(obs_index) %>%
    mutate(y_0 = first(y_hat),
           y_final = last(y_hat)) %>%
    ungroup() %>%
    select(ind_id, y_0, y_final) %>%
    distinct()

  individual_data <- left_join(individual_data, initial_and_final_vals, by="ind_id")

  #Generate plot
  plot <- hmde_ggplot_de_pieces(pars_data = individual_data,
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
                                  DE_function,
                                  xlab,
                                  ylab,
                                  title,
                                  colour,
                                  alpha){
  plot <- ggplot() +
    xlim(min(pars_data$y_0), max(pars_data$y_final)) +
    labs(x = xlab, y = ylab, title = title) +
    theme_classic() +
    theme(axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"))

  for(i in 1:nrow(pars_data)){
    args_list <- list(pars=pars_data[i,])
    plot <- plot +
      geom_function(fun=DE_function, args=args_list,
                    colour=colour, linewidth=1,
                    xlim=c(pars_data$y_0[i], pars_data$y_final[i]))
  }

  return(plot)
}
