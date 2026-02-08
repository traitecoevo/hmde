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
    obs_data = NA,
    prior_pars = NA
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
  if(names(value) != names(x@obs_data)){
    stop("List names do not match obs_data template.")
  }

  x@obs_data <- value
  x
})

#Prior paramerters
setGeneric("prior_pars", function(x) standardGeneric("prior_pars"))
setGeneric("prior_pars<-", function(x, value) standardGeneric("prior_pars<-"))
setMethod("prior_pars", "hmde_data_template", function(x) x@prior_pars)
setMethod("prior_pars<-", "hmde_data_template", function(x, value) {
  if(names(value) != names(x@prior_pars)){
    stop("Prior names do not match prior_pars template.")
  }

  x@prior_pars <- value
  x
})


#-----------------------------------------------------------------------------#
## Helper Functions

# Constructor
hmde_data_template <- function(model_name, #Mandatory
                               model_level = NA_character_, #Optional
                               obs_data = NA, #Optional
                               prior_pars = NA, #Optional
                               ...){  #Optional additional user input
  #Validation
  if(!model %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  template <- hmde_model(model_name) #Construct new object with defaults for model

  if(model_level(template) != model_level){
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
  if(!is.na(obs_data)){
    #Check for name structure
    if(names(obs_data) != data_fields_obs){
      stop("Input observation data do not have correct names.")
    }

    obs_data(template) <- obs_data
  } else { #Check if additional user fields are provided
    if(model_level(template) == "multi_ind"){ # Multi-individual with ind_id vec
      for(i in c("ind_id", "time", "y_obs", "obs_index")){
        if(!i %in% user_fields){
          stop(paste0("Improper data structure: ", i, " missing"))
        }
      }

    } else { # Single individual models
      for(i in c("time", "y_obs", "obs_index")){
        if(!i %in% user_fields){
          stop(paste0("Improper data structure: ", i, " missing"))
        }
      }
    }

    #Construct list formation
    obs_data_temp_list <- obs_data(template)
    for(i in data_fields_obs){ # Iterate through required fields and fill them
      if(i %in% user_fields){ #Check if the user has supplied it in a tibble
        obs_data_temp_list <- purrr::list_modify(obs_data_temp_list, !!!additional_data[i])

      }

      if(is.na(obs_data_temp_list[[i]])){ #Catches default tibble transformations
        obs_data_temp_list[[i]] <- switch(
          i,
          n_obs = length(obs_data_temp_list$y_obs),
          n_ind = length(unique(obs_data_temp_list$ind_id)),
          y_bar = mean(obs_data_temp_list$y_obs)
        )
      }

      if(is.null(obs_data_temp_list[[i]])){ #Report missing data
        stop(paste("Improper data structure: Data missing:", i))
      }
    }

    obs_data(template) <- obs_data_temp_list
  }

  if(!is.na(prior_pars)){
    #Check for name structure
    if(names(prior_pars) != data_fields_priors){
      stop("Input prior parameters do not have correct names.")
    }

    prior_pars(template) <- prior_pars

  #Check if non-default user fields are provided
  } else if(length(intersect(user_fields, data_fields_priors)) > 0){
    #Construct list formation
    prior_par_temp_list <- prior_pars(template) #Take default priors as base structure
    for(i in data_fields_priors){ # Iterate through required fields and update them
      if(i %in% user_fields){ #Check if the user has supplied it in a tibble
        #Validate prior parameter length
        if(length(!!!additional_data[i]) != length(data_fields_priors[[i]])){
          stop(paste0("Length of parameters for ", i,
                      " incorrect. Should be ", length(data_fields_priors[[i]]),
                      "."))
        }

        prior_par_temp_list <- purrr::list_modify(prior_par_temp_list, !!!additional_data[i])
      }
    }

    prior_pars(template) <- prior_par_temp_list
  }

  #Check validity
  validObject(template)

  return(template)
}

