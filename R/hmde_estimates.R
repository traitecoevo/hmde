#S4 methods for hmde_estimates object
# Class definition
setClass(
  Class = "hmde_estimates",
  representation = representation(
    model_name = "character", # Name of model DE
    model_level = "character", # Single or multi-individual
    method = "character", # Estimation method eg. sampling
    runtime = "matrix", # Runtime matrix
    summary = "character", # Summary of fit
    measurement_ests = "tbl_df", # Tibble of measurement-level estimates
    individual_ests = "tbl_df", # Tibble of individual-level parameters
    population_ests = "tbl_df", # Tibble of species-level estimates, empty if single ind
    error_ests = "tbl_df", # Tibble of error terms
    prior_pars = "list" # List of prior parameters for model
  ),
  prototype = prototype(
    model_name = NA_character_,
    model_level = NA_character_,
    measurement_ests = tibble(NA),
    individual_ests = tibble(NA),
    population_ests = tibble(NA),
    error_ests = tibble(NA),
    prior_pars = list(NA)
  )
)

# Validation
setValidity("hmde_data_template",
  function(object){
    if(length(object@model_name) != 1)
      return("'model_name' slot must be of length 1.")

    if(length(object@model_level) != 1)
      return("'model_level' slot must be of length 1.")

    TRUE
  }
)


#-----------------------------------------------------------------------------#
# Setters and getters for slots

#Model name
setGeneric("model_name", function(x) standardGeneric("model_name"))
setGeneric("model_name<-", function(x, value) standardGeneric("model_name<-"))
setMethod("model_name", "hmde_data_template", function(x) x@model_name)
setMethod("model_name<-", "hmde_data_template", function(x, value) {
  x@model_name <- value
  x
})

#Model level
setGeneric("model_level", function(x) standardGeneric("model_level"))
setGeneric("model_level<-", function(x, value) standardGeneric("model_level<-"))
setMethod("model_level", "hmde_data_template", function(x) x@model_level)
setMethod("model_level<-", "hmde_data_template", function(x, value) {
  x@model_level <- value
  x
})

#Measurement estimates
setGeneric("measurement_ests", function(x) standardGeneric("measurement_ests"))
setGeneric("measurement_ests<-", function(x, value) standardGeneric("measurement_ests<-"))
setMethod("measurement_ests", "hmde_estimates", function(x) x@measurement_ests)
setMethod("measurement_ests<-", "hmde_estimates", function(x, value) {
  x@measurement_ests <- value
  x
})

#Individual estimates
setGeneric("individual_ests", function(x) standardGeneric("individual_ests"))
setGeneric("individual_ests<-", function(x, value) standardGeneric("individual_ests<-"))
setMethod("individual_ests", "hmde_estimates", function(x) x@individual_ests)
setMethod("individual_ests<-", "hmde_estimates", function(x, value) {
  x@individual_ests <- value
  x
})

#Species estimates
setGeneric("population_ests", function(x) standardGeneric("population_ests"))
setGeneric("population_ests<-", function(x, value) standardGeneric("population_ests<-"))
setMethod("population_ests", "hmde_estimates", function(x) x@population_ests)
setMethod("population_ests<-", "hmde_estimates", function(x, value) {
  x@population_ests <- value
  x
})

#Error estimates
setGeneric("error_ests", function(x) standardGeneric("error_ests"))
setGeneric("error_ests<-", function(x, value) standardGeneric("error_ests<-"))
setMethod("error_ests", "hmde_estimates", function(x) x@error_ests)
setMethod("error_ests<-", "hmde_estimates", function(x, value) {
  x@population_ests <- value
  x
})

#Prior paramerters
setGeneric("prior_pars", function(x) standardGeneric("prior_pars"))
setGeneric("prior_pars<-", function(x, value) standardGeneric("prior_pars<-"))
setMethod("prior_pars", "hmde_data_template", function(x) x@prior_pars)
setMethod("prior_pars<-", "hmde_data_template", function(x, value) {
  x@prior_pars <- value
  x
})


#-----------------------------------------------------------------------------#
## Helper Functions

# Constructor
#' Constructor function for hmde_estimates class.
#'
#' @param fit stanfit class object
#' @param obs_data tbl_df class object with variables for ind_id, time, y_obs
#'
#' @return hmde_estimates class object
#'
#' @examples
#' # basic usage of hmde_data_template
#' hmde_data_template("constant_single_ind",
#'                    obs_data = Trout_Size_Data[1:4,]) |>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   hmde_estimates(obs_data = Trout_Size_Data[1:4,])
#'
#' @export
#' @import dplyr
#' @importFrom rstan get_elapsed_time
#' @importFrom stats quantile

hmde_data_template <- function(fit, #Mandatory
                               obs_data){ #Mandatory
  #Check fit class
  if(!inherits(fit, "stanfit")){
    stop("Fit object must be of class stanfit.")
  }

  model <- fit@model_name
  #Model
  if(!model %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  #Names of columns in obs_data
  for(i in c("ind_id", "time", "y_obs")){
    if(!i %in% names(obs_data)){
      stop(paste0("Variable ", i, " missing from obs_data."))
    }
  }

  #In case obs_index is not provided
  if(!"obs_index" %in% names(obs_data)){
    obs_data <- obs_data %>%
      group_by(ind_id) %>%
      arrange(time) %>%
      rownames_to_column(var = "obs_index") %>%
      ungroup()
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
  prior_data <- hmde_extract_prior_pars(samples, prior_names)

  #Extract measurement, individual-level, and error parameter estimates
  measurement_data <- hmde_extract_measurement_ests(
    samples,
    par_names$measurement_pars_names,
    input_measurement_data)

  individual_data <- hmde_extract_individual_par_ests(
    samples,
    par_names$individual_pars_names,
    n_ind)

  error_data <- hmde_extract_error_par_ests(
    samples,
    par_names$error_pars_names)

  #If model is multi-individual extract population-level estimates and add to list
  if(!is.null(par_names$population_pars_names)){
    population_data <- hmde_extract_pop_par_ests(samples,
                                                               population_pars_names =
                                                                 par_names$population_pars_names)
  }
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
