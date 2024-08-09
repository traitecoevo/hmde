#' Extract samples and return measurement, individual, and population-level estimates
#'
#' @param model model name character string
#' @param fit fitted model Stan fit
#' @param input_measurement_data data used to fit the model with ind_id, y_obs, time, obs_index tibble
#'
#' @return named list with data frames for measurement, individual, population-level, and error parameter estimates
#' @export

hmde_extract_samples <- function(model = NULL,
                                 fit = NULL,
                                 input_measurement_data = NULL){
  browser()
  estimate_list <- list()
  par_names <- hmde_model_pars(model)

  if(grepl("multi", model)){ #Get n_ind for multi-individual
    n_ind <- length(unique(input_measurement_data$ind_id))
  } else {
    n_ind <- 1
  }

  #Extract samples
  samples <- rstan::extract(fit, permuted = TRUE, inc_warmup = FALSE)

  #Extract measurement, individual-level, and error parameter estimates and add to list
  estimate_list$measurement_data <- hmde_extract_measurement_ests(samples,
                                                                    par_names$measurement_pars_names,
                                                                    input_measurement_data)

  estimate_list$individual_data <- hmde_extract_individual_par_ests(samples,
                                                                    par_names$individual_pars_names,
                                                                    n_ind)

  estimate_list$error_data <- hmde_extract_error_par_ests(samples, par_names$error_pars_names)

  #If model is multi-individual extract population-level estimates and add to list
  if(!is.null(par_names$population_pars_names)){
    estimate_list$population_data <- hmde_extract_pop_par_ests(samples,
                                                               par_names$population_pars_names)
  }

  return(estimate_list)
}


#' Sample extraction for measurement-level estimates
#' @keywords internal
#' @noRd
hmde_extract_measurement_ests <- function(samples = NULL,
                                          measurement_pars_names = NULL,
                                          input_measurement_data = NULL){
  measurement_data <- input_measurement_data

  for(i in measurement_pars_names){
    measurement_data[[i]] <- apply(samples[[i]], 2, mean)
  }

  return(measurement_data)
}

#' Sample extraction for individual-level parameters
#' @keywords internal
#' @noRd
hmde_extract_individual_par_ests <- function(samples = NULL,
                                             individual_pars_names = NULL,
                                             n_ind = NULL){
  individual_data <- tibble(ind_id = 1:n_ind)

  #Extract mean of parameter posterior distributions
  for(i in individual_pars_names){
    individual_data[[paste0(i, "_mean")]] <- apply(samples[[i]], 2, mean)
    individual_data[[paste0(i, "_median")]] <- apply(samples[[i]], 2, median)
    individual_data[[paste0(i, "_CI_lower")]] <- apply(samples[[i]], 2,
                                                       quantile, probs=c(0.025))
    individual_data[[paste0(i, "_CI_upper")]] <- apply(samples[[i]], 2,
                                                       quantile, probs=c(0.975))
  }

  return(individual_data)
}

#' #' Sample extraction for population-level parameters
#' @keywords internal
#' @noRd
hmde_extract_pop_par_ests <- function(samples = NULL,
                                      population_pars_names = NULL){
  browser()
  population_data <- tibble()

  #Extract mean of parameter posterior distributions
  for(i in population_pars_names){
    pop_data_temp <- tibble(par_name = i)
    pop_data_temp[["mean"]] <-  mean(samples[[i]])
    pop_data_temp[["median"]] <- median(samples[[i]])
    pop_data_temp[["CI_lower"]] <- as.numeric(quantile(samples[[i]],
                                                       probs=c(0.025)))
    pop_data_temp[["CI_upper"]] <- as.numeric(quantile(samples[[i]],
                                                       probs=c(0.975)))

    rbind(population_data, pop_data_temp)
  }

  return(population_data)

}

#' #' Sample extraction for error parameters
#' @keywords internal
#' @noRd
hmde_extract_error_par_ests <- function(samples = NULL,
                                        error_pars_names = NULL){
  error_data <- tibble()

  #Extract mean of parameter posterior distributions
  for(i in error_pars_names){
    error_data_temp <- tibble(par_name = i)
    error_data_temp[["mean"]] <- mean(samples[[i]])
    error_data_temp[["median"]] <- median(samples[[i]])
    error_data_temp[["CI_lower"]] <- as.numeric(quantile(samples[[i]],
                                                         probs=c(0.025)))
    error_data_temp[["CI_upper"]] <- as.numeric(quantile(samples[[i]],
                                                         probs=c(0.975)))

    error_data <- rbind(error_data, error_data_temp)
  }

  return(error_data)
}
