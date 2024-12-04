#' Plot estimated and observed values over time for a chosen number of individuals based
#' on posterior estimates. Structured to take in the measurement_data tibble constructed by
#' the hmde_extract_estimates function.
#'
#' @param model model name character string
#' @param measurement_data tibble with estimated measurements
#' @param ind_id_vec vector with list of ind_id values
#' @param n_ind_to_plot integer giving number of individuals to plot if not speciried
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

hmde_plot_obs_est_inds <- function(ind_id_vec = NULL,
                                   n_ind_to_plot = NULL,
                                   measurement_data = NULL,
                                   xlab = "Time",
                                   ylab = "Y(t)",
                                   title = NULL){
  if(is.null(measurement_data)){
    stop("Measurement data not provided.")
  }

  if(!is.null(ind_id_vec)){
    for(i in ind_id_vec){ #Error if ID nujmber not in data.
      if(!i %in% measurement_data$ind_id){
        stop(paste0("Ind ID values not recognised: ", i))
      }
    }

    plot_data <- measurement_data %>%
      filter(ind_id %in% ind_id_vec)

  } else if(is.null(n_ind_to_plot)){ #Error if no info for which inds to plot
    stop("Neither ind. ID values nor a number of individuals provided.")

  } else if(length(unique(measurement_data$ind_id)) < n_ind_to_plot){
    stop("Number of individuals to plot larger than sample size, please provide a smaller number.")

  } else {
    sample_ids <- sample(unique(measurement_data$ind_id), size=n_ind_to_plot)
    plot_data <- measurement_data %>%
      filter(ind_id %in% sample_ids)
  }

  #Generate plot
  plot <- hmde_ggplot_obs_est_inds(plot_data = plot_data,
                                xlab = xlab,
                                ylab = ylab,
                                title = title)

  return(plot)
}

#' Produce plot for plot_obs_est_inds
#' @keywords internal
#' @noRd
hmde_ggplot_obs_est_inds <- function(plot_data,
                                     xlab,
                                     ylab,
                                     title){
  plot <- ggplot(data=plot_data, aes(group = ind_id)) +
    geom_point(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
               shape = 1) +
    geom_line(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
              linetype = "dashed") +
    geom_point(aes(x = time, y=y_hat, colour = as.factor(ind_id)),
               shape = 2) +
    geom_line(aes(x = time, y=y_hat, colour = as.factor(ind_id)),
              linetype = "solid") +
    labs(x=xlab, y=ylab, colour="Ind. ID") +
    theme_classic()

  return(plot)
}
