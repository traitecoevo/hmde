#S4 methods for hmde_estimates object
# Class definition
setClass(
  Class = "hmde_estimates",
  representation = representation(
    model_name = "character", # Name of model DE
    model_level = "character", # Single or multi-individual
    method = "character", # Estimation method eg. sampling
    runtime = "matrix", # Runtime matrix
    fit_summary = "character", # Summary of fit
    measurement_ests = "tbl_df", # Tibble of measurement-level estimates
    individual_ests = "tbl_df", # Tibble of individual-level parameters
    population_ests = "tbl_df", # Tibble of species-level estimates, empty if single ind
    error_ests = "tbl_df", # Tibble of error terms
    prior_pars = "list", # List of prior parameters for model
    par_names = "list" # List of model parameter names
  ),
  prototype = prototype(
    model_name = NA_character_,
    model_level = NA_character_,
    method = NA_character_,
    runtime = matrix(NA),
    fit_summary = NA_character_,
    measurement_ests = tibble(NA),
    individual_ests = tibble(NA),
    population_ests = tibble(NA),
    error_ests = tibble(NA),
    prior_pars = list(NA),
    par_names = list(NA)
  )
)

# Validation
setValidity("hmde_estimates",
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
#Generics set by hmde_model_template
#' Getter for model_name in hmde_estimates object
#' @param x hmde_estimates class object
#' @return character string
#' @export
setMethod("model_name", signature = "hmde_estimates", function(x) x@model_name)

#' Setter for model_name in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value character string
#' @return hmde_estimates class object
#' @export
setMethod("model_name<-", signature = "hmde_estimates", function(x, value) {
  x@model_name <- value
  x
})

#Model level
#Generics set by hmde_model_template
#' Getter for model_level in hmde_estimates object
#' @param x hmde_estimates class object
#' @return character string
#' @export
setMethod("model_level", signature = "hmde_estimates", function(x) x@model_level)

#' Setter for model_level in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value character string
#' @return hmde_estimates class object
#' @export
setMethod("model_level<-", signature = "hmde_estimates", function(x, value) {
  x@model_level <- value
  x
})

#Method
setGeneric("method", function(x) standardGeneric("method"))
setGeneric("method<-", function(x, value) standardGeneric("method<-"))

#' Getter for method in hmde_estimates object
#' @param x hmde_estimates class object
#' @return character string
#' @export
setMethod("method", signature = "hmde_estimates", function(x) x@method)

#' Setter for method in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value character string
#' @return hmde_estimates class object
#' @export
setMethod("method<-", signature = "hmde_estimates", function(x, value) {
  x@method <- value
  x
})

#Runtime
setGeneric("runtime", function(x) standardGeneric("runtime"))
setGeneric("runtime<-", function(x, value) standardGeneric("runtime<-"))

#' Getter for runtime in hmde_estimates object
#' @param x hmde_estimates class object
#' @return numeric matrix
#' @export
setMethod("runtime", signature = "hmde_estimates", function(x) x@runtime)

#' Setter for runtime in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value numeric matrix
#' @return hmde_estimates class object
#' @export
setMethod("runtime<-", signature = "hmde_estimates", function(x, value) {
  x@runtime <- value
  x
})

#Fit summary
setGeneric("fit_summary", function(x) standardGeneric("fit_summary"))
setGeneric("fit_summary<-", function(x, value) standardGeneric("fit_summary<-"))

#' Getter for fit_summary in hmde_estimates object
#' @param x hmde_estimates class object
#' @return character string
#' @export
setMethod("fit_summary", signature = "hmde_estimates", function(x) x@fit_summary)

#' Setter for fit_summary in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value character string
#' @return hmde_estimates class object
#' @export
setMethod("fit_summary<-", signature = "hmde_estimates", function(x, value) {
  x@fit_summary <- value
  x
})

#Measurement estimates
setGeneric("measurement_ests", function(x) standardGeneric("measurement_ests"))
setGeneric("measurement_ests<-", function(x, value) standardGeneric("measurement_ests<-"))

#' Getter for measurement_ests in hmde_estimates object
#' @param x hmde_estimates class object
#' @return tibble of estimates
#' @export
setMethod("measurement_ests", signature = "hmde_estimates", function(x) x@measurement_ests)

#' Setter for measurement_ests in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value tibble of estimates
#' @return hmde_estimates class object
#' @export
setMethod("measurement_ests<-", signature = "hmde_estimates", function(x, value) {
  x@measurement_ests <- value
  x
})

#Individual estimates
setGeneric("individual_ests", function(x) standardGeneric("individual_ests"))
setGeneric("individual_ests<-", function(x, value) standardGeneric("individual_ests<-"))

#' Getter for individual_ests in hmde_estimates object
#' @param x hmde_estimates class object
#' @return tibble of estimates
#' @export
setMethod("individual_ests", signature = "hmde_estimates", function(x) x@individual_ests)

#' Setter for individual_ests in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value tibble of estimates
#' @return hmde_estimates class object
#' @export
setMethod("individual_ests<-", signature = "hmde_estimates", function(x, value) {
  x@individual_ests <- value
  x
})

#Population estimates
setGeneric("population_ests", function(x) standardGeneric("population_ests"))
setGeneric("population_ests<-", function(x, value) standardGeneric("population_ests<-"))

#' Getter for population_ests in hmde_estimates object
#' @param x hmde_estimates class object
#' @return tibble of estimates
#' @export
setMethod("population_ests", signature = "hmde_estimates", function(x) x@population_ests)

#' Setter for population_ests in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value tibble of estimates
#' @return hmde_estimates class object
#' @export
setMethod("population_ests<-", signature = "hmde_estimates", function(x, value) {
  x@population_ests <- value
  x
})

#Error estimates
setGeneric("error_ests", function(x) standardGeneric("error_ests"))
setGeneric("error_ests<-", function(x, value) standardGeneric("error_ests<-"))

#' Getter for error_ests in hmde_estimates object
#' @param x hmde_estimates class object
#' @return tibble of estimates
#' @export
setMethod("error_ests", signature = "hmde_estimates", function(x) x@error_ests)

#' Setter for error_ests in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value tibble of estimates
#' @return hmde_estimates class object
#' @export
setMethod("error_ests<-", signature = "hmde_estimates", function(x, value) {
  x@error_ests <- value
  x
})

#Prior paramerters
#Generics set by hmde_model_template
#' Getter for prior_pars in hmde_estimates object
#' @param x hmde_estimates class object
#' @return list of named vectors
#' @export
setMethod("prior_pars", signature = "hmde_estimates", function(x) x@prior_pars)

#' Setter for prior_pars in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value list of named vectors
#' @return hmde_estimates class object
#' @export
setMethod("prior_pars<-", signature = "hmde_estimates", function(x, value) {
  x@prior_pars <- value
  x
})

#Parameter names
#Generics set by hmde_model_template
#' Getter for par_names in hmde_estimates object
#' @param x hmde_estimates class object
#' @return vector of character strings
#' @export
setMethod("par_names", signature = "hmde_estimates", function(x) x@par_names)

#' Setter for par_names in hmde_estimates object
#' @param x hmde_estimates class object
#' @param value vector of character strings
#' @return hmde_estimates class object
#' @export
setMethod("par_names<-", signature = "hmde_estimates", function(x, value) {
  x@par_names <- value
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
#' # basic usage of hmde_estimates
#' hmde_data_template("constant_single_ind",
#'                    obs_data = Trout_Size_Data[1:4,]) |>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   hmde_estimates(obs_data = Trout_Size_Data[1:4,])
#'
#' @export
#' @import dplyr
#' @import tibble
#' @importFrom rstan get_elapsed_time
#' @importFrom stats quantile

hmde_estimates <- function(fit, #Mandatory
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
      mutate(obs_index = rank(time)) %>%
      ungroup()
  }

  #Get model level
  if(grepl("multi_ind", model)){
    model_level <- "multi_ind"
  } else if (grepl("single_ind", model)){
    model_level <- "single_ind"
  } else {
    stop("Model level not identified.")
  }

  #Extract info from fit object
  method <- fit@stan_args[[1]][["method"]]
  if(method == "sampling"){
    fit_summary <- paste0(
      "Method: MCMC sampling with ",
      fit@stan_args[[1]][["algorithm"]],
      " algorithm",
      "\nChains: ", fit@sim[["chains"]],
      "\nIterations: ", fit@sim[["iter"]],
      "\nWarmup: ", fit@sim[["warmup"]]
    )
  }

  runtime <- rstan::get_elapsed_time(fit)

  estimate_object <- new("hmde_estimates",
             model_name = model,
             model_level = model_level,
             method = method,
             fit_summary = fit_summary,
             runtime = runtime,
             par_names = hmde_model_pars(model))

  par_names <- par_names(estimate_object)
  prior_names <-
    paste0("check_",
           names(prior_pars(hmde_data_template(model)))
    )

  if(grepl("multi", model)){ #Get n_ind for multi-individual
    n_ind <- length(unique(obs_data$ind_id))
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
  prior_pars(estimate_object) <-
    hmde_extract_prior_pars(samples, prior_names)

  #Extract measurement, individual-level, and error parameter estimates
  measurement_ests(estimate_object) <-
    hmde_extract_measurement_ests(
      samples,
      par_names$measurement_pars_names,
      obs_data)

  individual_ests(estimate_object) <-
    hmde_extract_individual_par_ests(
      samples,
      par_names$individual_pars_names,
      n_ind)

  error_ests(estimate_object) <-
    hmde_extract_error_par_ests(
      samples,
      par_names$error_pars_names)

  #If model is multi-individual extract population-level estimates and add to list
  if(!is.null(par_names$population_pars_names)){
    population_ests(estimate_object) <-
      hmde_extract_pop_par_ests(samples,
                                population_pars_names =
                                  par_names$population_pars_names)
  }

  #Validate
  if(!validObject(estimate_object)){
    stop("Invalid estimates object")
  }

  return(estimate_object)
}


#' Sample extraction for measurement-level estimates
#' @keywords internal
#' @noRd
hmde_extract_measurement_ests <- function(samples = NULL,
                                          measurement_pars_names = NULL,
                                          obs_data = NULL){
  measurement_data <- obs_data

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

#----------------------------------------------------------------------
## Generic functions for show, print, summary, plot

#' Show function for hmde_estimates object
#' @param object hmde_estimates class output
#'
#' @examples
#' # basic usage of show
#' hmde_data_template("constant_single_ind",
#'   obs_data = Trout_Size_Data[1:4,]) |>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   hmde_estimates(Trout_Size_Data[1:4,]) |>
#'   show()
#'
#' @importFrom knitr kable
#'
#' @export

setMethod("show", signature = "hmde_estimates", function(object) {
  #Outputs model info and top level parameter table

  if(grepl("multi", model_name(object))){
    writeLines(paste0(
      "Model name: ", model_name(object),
      "\nModel level: multi-individual",
      "\nTop level: population",
      "\n", fit_summary(object)))

    writeLines(
      "\nTop level parameter estimates:"
    )
    writeLines(knitr::kable(population_ests(object),
                            format = "markdown", digits = 3))

  } else if(grepl("single", model_name(object))){
    writeLines(paste0(
      "Model name: ", model_name(object),
      "\nModel level: single individual",
      "\nTop level: individual",
      "\nTop level parameter estimates:",
      "\n", fit_summary(object)))

    writeLines(
      "\nTop level parameter estimates:"
    )
    writeLines(knitr::kable(individual_ests(object),
                            format = "markdown", digits = 3))
  }

  writeLines("\nRuntime information:")
  writeLines(knitr::kable(runtime(object),
                          format = "markdown", digits = 3))
})


#' Print function for hmde_estimates object
#'
#' @param x hmde_estimates class output
#'
#' @examples
#' # basic usage of print
#' hmde_data_template("constant_single_ind",
#'   obs_data = Trout_Size_Data[1:4,]) |>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   hmde_estimates(Trout_Size_Data[1:4,]) |>
#'   print()
#'
#' @importFrom knitr kable
#'
#' @export

setMethod("print", signature = "hmde_estimates", function(x) {
  #Outputs model info and top level parameter table

  if(grepl("multi", model_name(x))){
    writeLines(paste0(
      "Model name: ", model_name(x),
      "\nModel level: multi-individual",
      "\nTop level: population",
      "\n", fit_summary(x)))

    writeLines(
      "\nTop level parameter estimates:"
    )
    writeLines(knitr::kable(population_ests(x),
                            format = "markdown", digits = 3))

  } else if(grepl("single", model_name(x))){
    writeLines(paste0(
      "Model name: ", model_name(x),
      "\nModel level: single individual",
      "\nTop level: individual",
      "\nTop level parameter estimates:",
      "\n", fit_summary(x)))

    writeLines(
      "\nTop level parameter estimates:"
    )
    writeLines(knitr::kable(individual_ests(x),
                            format = "markdown", digits = 3))
  }

  writeLines("\nRuntime information:")
  writeLines(knitr::kable(runtime(x),
                          format = "markdown", digits = 3))
})


#' Summary function for hmde_estimates object
#'
#' @param object hmde_estimates class object
#'
#' @examples
#' # basic usage of summary
#' hmde_data_template("constant_single_ind",
#'   obs_data = Trout_Size_Data[1:4,]) |>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   hmde_estimates(Trout_Size_Data[1:4,]) |>
#'   summary()
#'
#' @importFrom knitr kable
#'
#' @export

setMethod("summary", signature = "hmde_estimates", function(object) {
  #Outputs model info and top level parameter table

  if(grepl("multi", model_name(object))){
    writeLines(paste0(
      "Model name: ", model_name(object),
      "\nModel level: multi-individual",
      "\nTop level: population",
      "\n", fit_summary(object)))

    writeLines(
      "\nTop level parameter estimates:"
    )
    writeLines(knitr::kable(population_ests(object),
                            format = "markdown", digits = 3))

  } else if(grepl("single", model_name(object))){
    writeLines(paste0(
      "Model name: ", model_name(object),
      "\nModel level: single individual",
      "\nTop level: individual",
      "\nTop level parameter estimates:",
      "\n", fit_summary(object)))

    writeLines(
      "\nTop level parameter estimates:"
    )
    writeLines(knitr::kable(individual_ests(object),
                            format = "markdown", digits = 3))
  }
})

#' Plot function for hmde_estimates object
#'
#' @param x hmde_estimates class object
#'
#' @examples
#' # basic usage of print
#' hmde_data_template("constant_single_ind",
#'   obs_data = Trout_Size_Data[1:4,]) |>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   hmde_estimates(Trout_Size_Data[1:4,]) |>
#'   plot()
#'
#' @importFrom cowplot plot_grid
#'
#' @export

setMethod("plot", signature = "hmde_estimates", function(x) {
  plot_1 <- hmde_plot_de_pieces(x) +
    labs(title = "Plot of DE pieces fit to each individual")

  if(grepl("multi", model_name(x))){
    n_ind_plot <- min(5, nrow(individual_ests(x)))
    plot_2 <- hmde_plot_obs_est_inds(x, n_ind_to_plot = n_ind_plot) +
      labs(title = "Plot of fit sizes over time")
  } else {
    plot_2 <- hmde_plot_obs_est_inds(x, n_ind_to_plot = 1) +
      labs(title = "Plot of fit sizes over time")
  }

  return_plot <- plot_grid(plot_1, plot_2, nrow = 2, align = "v")
  return(return_plot)
})
