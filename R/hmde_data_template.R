#S4 methods for hmde_data_template

# Class definition
setClass(
  Class = "hmde_data_template",
  representation = representation(
    model_name = "character", # Name of model DE
    model_level = "character", # Single or multi-individual
    obs_data = "list", # List of input data for model
    prior_pars = "list" # List of prior parameters for model
  ),
  prototype = prototype(
    model_name = NA_character_,
    model_level = NA_character_,
    obs_data = list(NA),
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

#Observation data
setGeneric("obs_data", function(x) standardGeneric("obs_data"))
setGeneric("obs_data<-", function(x, value) standardGeneric("obs_data<-"))
setMethod("obs_data", "hmde_data_template", function(x) x@obs_data)
setMethod("obs_data<-", "hmde_data_template", function(x, value) {
  x@obs_data <- value
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
#' Constructor function for hmde_data_template class, with data assignment ready for model fitting.
#'
#' @param model_name character string name of a hmde model
#' @param model_level character string specifying whether single or multiple inds
#' @param obs_data list containing observational data vectors
#' @param prior_pars list containing prior parameters
#' @param ... data-masking name-value pairs allowing specific input of elements
#'
#' @return hmde_data_template class object
#'
#' @examples
#' # basic usage of hmde_data_template
#' hmde_data_template("constant_single_ind")
#'
#' # basic usage of hmde_data_template with data input
#' hmde_data_template("constant_single_ind",
#'   obs_data = list(
#'     n_obs = 3,
#'     y_obs = c(1,1,1),
#'     obs_index = 1:3,
#'     time = 0:2
#'   )
#' )
#'
#' #basic usage of hmde_data_template with prior input
#' hmde_data_template("constant_single_ind",
#'   prior_pars = list(
#'     prior_pars_ind_beta = c(1,2),
#'     prior_pars_global_error_sigma = c(1,3)
#'   )
#' )
#'
#' @export
#'
hmde_data_template <- function(model_name, #Mandatory
                               model_level = NA_character_, #Optional
                               obs_data = NULL, #Optional
                               prior_pars = NULL, #Optional
                               ...){  #Optional additional user input
  #Validation
  if(!model_name %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  template <- hmde_model(model_name) #Construct new object with defaults for model

  if(!is.na(model_level) && (model_level(template) != model_level)){
    stop("Provided model level does not agree with model name.")
  }

  #Get additional user input
  user_code <- rlang::enquos(..., .check_assign = TRUE)
  if(length(user_code) != 0){
    user_fields <- names(user_code)
    # Evaluate the RHS of expressions (the values)
    additional_data <- purrr::map(user_code,
                                  ~rlang::eval_tidy(.x, env = rlang::caller_env())
    )
  }

  data_fields_obs <- names(obs_data(template))
  data_fields_priors <- names(prior_pars(template))

  #If obs_data is provided, check it has required names
  if(!is.null(obs_data)){
    #Check for name structure
    if(!identical(names(obs_data), data_fields_obs)){
      stop("Input observation data do not have correct names.")
    }

    obs_data(template) <- obs_data
  } else if(length(user_code) != 0
            && length(intersect(data_fields_obs, user_fields)) > 0
            ){ #Check if relevant user fields are provided
    #Construct list formation
    obs_data_temp_list <- obs_data(template)
    for(i in data_fields_obs){ # Iterate through required fields and fill them
      if(i %in% user_fields){ #Check if the user has supplied it in a tibble
        obs_data_temp_list <- purrr::list_modify(obs_data_temp_list,
                                                 !!!additional_data[i])
      }
    }

    # Check that fields are valid and filled.
    for(i in intersect(data_fields_obs, c("n_obs", "n_ind", "y_bar"))){
      if(length(obs_data_temp_list[[i]]) == 1){
        if(is.na(obs_data_temp_list[[i]])){ #Catches default tibble transformations

          obs_data_temp_list[[i]] <- switch(
            i,
            n_obs = length(obs_data_temp_list$y_obs),
            n_ind = length(unique(obs_data_temp_list$ind_id)),
            y_bar = mean(obs_data_temp_list$y_obs)
          )
        }

        if(is.na(obs_data_temp_list[[i]])){ #Report missing data
          stop(paste("Improper data structure: Data missing:", i))
        }
      }
    }

    #Validate that vectors have content
    for(i in names(obs_data_temp_list)){
      if((i != "n_obs") && length(obs_data_temp_list[[i]]) == 1){
        if(is.na(obs_data_temp_list[[i]])){
          stop(paste("Data missing:", i))
        }
      }
    }

    #Validate lengths of vectors
    check_length <- obs_data_temp_list[["n_obs"]]
    for(i in names(obs_data_temp_list)){
      if(i != "n_obs"){
        if(length(obs_data_temp_list[[i]]) != check_length){
          stop(paste("Mismatch in length between n_obs = ",
                     check_length,  "and", i,
                     "with length",
                     length(obs_data_temp_list[[i]])))
        }
      }
    }

    obs_data(template) <- obs_data_temp_list
  }

  if(!is.null(prior_pars)){
    #Check for name structure
    if(!identical(names(prior_pars), data_fields_priors)){
      stop("Input prior parameters do not have correct names.")
    }

    #Check for vector lengths
    for(i in data_fields_priors){
      if(length(prior_pars[[i]]) != length(prior_pars(template)[[i]])){
        stop(paste("Input priors do not have correct lengths:", i))
      }
    }

    prior_pars(template) <- prior_pars

  #Check if non-default user fields are provided
  } else if(length(user_code) != 0
            && length(intersect(data_fields_priors, user_fields)) > 0){
    if(length(intersect(user_fields, data_fields_priors)) > 0){
      #Construct list formation
      prior_par_temp_list <- prior_pars(template) #Take default priors as base structure

      for(i in data_fields_priors){ # Iterate through required fields and update them
        if(i %in% user_fields){ #Check if the user has supplied values
          prior_par_temp_list <- purrr::list_modify(prior_par_temp_list, !!!additional_data[i])

          #Validate prior parameter length
          if(length(prior_par_temp_list[[i]]) != length(template@prior_pars[[i]])){
            stop(paste0("Length of parameters for ", i,
                        " incorrect. Should be ", length(template@prior_pars[[i]]),
                        "."))
          }
        }
      }

      prior_pars(template) <- prior_par_temp_list
    }
  }

  #Check validity
  validObject(template)

  return(template)
}

#----------------------------------------------------------------------------#
## Generic functions for show, print, summary, plot

#' Show function for hmde_data_template object
#'
#' @param x hmde_data_template class object
#'
#' @examples
#' # basic usage of show
#' hmde_data_template("constant_single_ind") |> show()
#'
#' @export

setMethod("show", "hmde_data_template", function(object) {
  cat(is(object)[[1]], "\n",
      "  Model name: ", object@model_name, "\n",
      "  Model level:  ", object@model_level, "\n",
      "  Input data names:  ", paste(names(object@obs_data),
                                     collapse = ", "), "\n",
      "  Prior names:  ", paste(names(object@prior_pars),
                                collapse = ", "), "\n",
      sep = ""
  )
})


#' Print function for hmde_data_template object
#'
#' @param x hmde_data_template class object
#'
#' @examples
#' # basic usage of print
#' hmde_data_template("constant_single_ind") |> print()
#'
#' @export

setMethod("print", "hmde_data_template", function(x) {
  cat(is(x)[[1]], "\n",
      "  Model name: ", x@model_name, "\n",
      "  Model level:  ", x@model_level, "\n",
      "  Input data names:  ", paste(names(x@obs_data),
                                     collapse = ", "), "\n",
      "  Prior names:  ", paste(names(x@prior_pars),
                                collapse = ", "), "\n",
      sep = ""
  )
})


#' Summary function for hmde_data_template object
#'
#' @param object hmde_data_template class object
#'
#' @examples
#' # basic usage of summary
#' hmde_data_template("constant_single_ind") |> summary()
#'
#' @export

setMethod("summary", "hmde_data_template", function(object) {
  if(!is.na(object@obs_data[[1]])){
    if(!is.null(object@obs_data[["ind_id"]])){
      summary_input_data <- head(tibble(
        obs_index = object@obs_data[["obs_index"]],
        ind_id = object@obs_data[["ind_id"]],
        y_obs = object@obs_data[["y_obs"]],
        time = object@obs_data[["time"]]
      ))

    } else {
      summary_input_data <- head(tibble(
        obs_index = object@obs_data[["obs_index"]],
        y_obs = object@obs_data[["y_obs"]],
        time = object@obs_data[["time"]]
      ))
    }

    # Output to console
    cat(is(object)[[1]], "\n",
        "  Model name: ", object@model_name, "\n",
        "  Model level:  ", object@model_level, "\n",
        "  Prior names:  ", paste(names(object@prior_pars),
                                  collapse = ", "), "\n",
        "  Observation data:  ", "\n",
        sep = ""
    )
    print(summary_input_data)

  } else {
    summary_input_data <- paste("  Input data names:  ",
                                paste(names(object@obs_data),
                                      collapse = ", "), "\n",
                                collapse = "")

    cat(is(object)[[1]], "\n",
        "  Model name: ", object@model_name, "\n",
        "  Model level:  ", object@model_level, "\n",
        summary_input_data,
        "  Prior names:  ", paste(names(object@prior_pars),
                                  collapse = ", "), "\n",
        sep = ""
    )
  }
})

#' Plot function for hmde_data_template object
#'
#' @param x hmde_data_template class object
#'
#' @examples
#' # basic usage of plot
#' hmde_model("constant_single_ind") |> plot()
#'
#' @export

setMethod("plot", "hmde_data_template", function(x) {
  model_pars_names <- hmde_model_pars(x@model_name)$individual_pars_names

  temp <- c()
  #Get parameter estimates from mean of prior distribution
  for(i in 1:length(model_pars_names)){
    #Get prior name
    if(grepl("multi", x@model_name, fixed = TRUE)){ #If multi-ind model
      prior_name <- paste0("prior_pars_pop_log_",
                           gsub("ind_", "", model_pars_names[i]), "_mean")
    } else { #Single individual model
      prior_name <- paste0("prior_pars_", model_pars_names[i])
    }

    #Cover von Bertalanffy prior centering at provided data max
    if(grepl("vb", x@model_name, fixed = TRUE) &&
       grepl("max_size", model_pars_names[i], fixed = TRUE)){
      temp[i+1] <- 10
    } else {
      temp[i+1] <- exp(x@prior_pars[[prior_name]][1])
    }
  }
  plot_pars <- rbind(temp)

  #Get final size for plot
  if(grepl("vb", x@model_name, fixed = TRUE)){
    y_final <- plot_pars[1,2]
  } else {
    y_final <- 20
  }

  #Plot ODE using parameter estimates
  plot <- hmde_ggplot_de_pieces(pars_data = plot_pars,
                                y_0 = 0,
                                y_final = y_final,
                                DE_function = hmde_model_des(x@model_name),
                                xlab = "Size",
                                ylab = "Growth rate",
                                title = paste0("Example ODE for ", x@model_name),
                                colour = "#006600",
                                alpha = 0.4)

  return(plot)
})
