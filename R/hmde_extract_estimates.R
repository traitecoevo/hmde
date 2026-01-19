#' Constructor function for hmde_estimates object. Extract samples and return measurement, individual, and population-level estimates
#'
#' @param fit fitted model Stan fit
#' @param input_measurement_data data used to fit the model with ind_id, y_obs, time, obs_index tibble
#'
#' @return hmde_estimates object comprising a named list with model information, data frames for measurement, individual, population-level, and error parameter estimates
#'
#' @examples
#' # basic usage of hmde_extract_estimates
#' hmde_model("constant_single_ind") |>
#'   hmde_assign_data(Trout_Size_Data)|>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   hmde_extract_estimates(Trout_Size_Data)
#'
#' @export
#' @import dplyr
#' @importFrom rstan get_elapsed_time
#' @importFrom stats quantile

hmde_extract_estimates <- function(fit = NULL,
                                   input_measurement_data = NULL){
  #Check for fit
  if(is.null(fit)){
    stop("Fit not provided.")
  }

  if(typeof(fit) != "S4"){
    stop("Fit not S4 stanfit type.")
  }

  model <- fit@model_name
  #Check for model
  if(!model %in% hmde_model_names()){
    stop(paste0("Model name not recognised: ", model,
                " Run hmde_model_names() to see available models."))
  }

  #Check for input measurement data
  for(i in c("y_obs", "time", "obs_index")){
    if(!i %in% names(input_measurement_data)){
      stop(paste("Input measurements information missing:", i))
    }
  }

  #Extract info from fit object
  method <- fit@stan_args[[1]][["method"]]
  if(method == "sampling"){
    summary <- paste0(
      "Method: MCMC sampling with ",
      fit@stan_args[[1]][["algorithm"]],
      " algorithm",
      "\nChains: ", fit@sim[["chains"]],
      "\nIterations: ", fit@sim[["iter"]],
      "\nWarmup: ", fit@sim[["warmup"]]
    )
  }

  runtime <- rstan::get_elapsed_time(fit)

  estimate_list <- list(model_name = model,
                        summary = summary,
                        runtime = runtime)
  par_names <- hmde_model_pars(model)
  prior_names <-
    paste0("check_",
           names(hmde_model(model =
                              model))[grep("prior_pars",
                                           names(hmde_model(model = model)))]
  )

  if(grepl("multi", model)){ #Get n_ind for multi-individual
    n_ind <- length(unique(input_measurement_data$ind_id))
  } else {
    n_ind <- 1
  }

  #Extract samples
  samples <- rstan::extract(fit, permuted = TRUE, inc_warmup = FALSE)
  sample_par_names <- names(samples)

  #Check parameter names
  for(i in par_names[1:(length(par_names)-1)]){
    for(j in i){
      if(!j %in% sample_par_names){
        stop(paste(
          "Parameter missing from model:", j
        ))
      }
    }
  }

  #Prior parameters
  estimate_list$prior_data <- hmde_extract_prior_pars(samples, prior_names)

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
                                                               population_pars_names =
                                                                 par_names$population_pars_names)
  }

  class(estimate_list) <- "hmde_estimates"

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

  if(n_ind > 1){
    for(i in individual_pars_names){
      individual_data[[paste0(i, "_mean")]] <- apply(samples[[i]], 2, mean)
      individual_data[[paste0(i, "_median")]] <- apply(samples[[i]], 2, median)
      individual_data[[paste0(i, "_CI_lower")]] <- apply(samples[[i]], 2,
                                                         stats::quantile, probs=c(0.025))
      individual_data[[paste0(i, "_CI_upper")]] <- apply(samples[[i]], 2,
                                                         stats::quantile, probs=c(0.975))
    }
  } else {
    for(i in individual_pars_names){
      individual_data[[paste0(i, "_mean")]] <- mean(samples[[i]])
      individual_data[[paste0(i, "_median")]] <- median(samples[[i]], 2, )
      individual_data[[paste0(i, "_CI_lower")]] <- as.numeric(stats::quantile(samples[[i]],
                                                            probs=c(0.025)))
      individual_data[[paste0(i, "_CI_upper")]] <- as.numeric(stats::quantile(samples[[i]],
                                                            probs=c(0.975)))
    }
  }


  return(individual_data)
}

#' #' Sample extraction for population-level parameters
#' @keywords internal
#' @noRd
hmde_extract_pop_par_ests <- function(samples = NULL,
                                      population_pars_names = NULL){
  population_data <- tibble()

  #Extract mean of parameter posterior distributions
  for(i in population_pars_names){
    pop_data_temp <- tibble(par_name = i)
    pop_data_temp$mean <-  mean(samples[[i]])
    pop_data_temp$median <- median(samples[[i]])
    pop_data_temp$CI_lower <- as.numeric(stats::quantile(samples[[i]],
                                                       probs=c(0.025)))
    pop_data_temp$CI_upper <- as.numeric(stats::quantile(samples[[i]],
                                                       probs=c(0.975)))

    population_data <- rbind(population_data, pop_data_temp)
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
    error_data_temp[["CI_lower"]] <- as.numeric(stats::quantile(samples[[i]],
                                                         probs=c(0.025)))
    error_data_temp[["CI_upper"]] <- as.numeric(stats::quantile(samples[[i]],
                                                         probs=c(0.975)))

    error_data <- rbind(error_data, error_data_temp)
  }

  return(error_data)
}

#' #' Prior parameter extraction
#' @keywords internal
#' @noRd
hmde_extract_prior_pars <- function(samples = NULL,
                                    prior_names = NULL){
  prior_data <- list()

  #Extract prior parameter value from the samples.
  for(i in prior_names){
    if(length(dim(samples[[i]])) > 1){
      prior_data[[sub("check_", "", i)]] <- list(par_name = i,
                              prior_vals = samples[[i]][1,])
    } else {
      prior_data[[sub("check_", "", i)]] <- list(par_name = i,
                              prior_vals = samples[[i]][1])
    }
  }

  return(prior_data)
}
