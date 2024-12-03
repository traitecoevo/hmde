#' Assign data to template for chosen model
#'
#' @param model_template output from hmde_model
#' @param data Input data tibble with columns including time, y_obs, obs_index, and additionally ind_id for multi-individual models
#' @param ... data-masking name-value pairs allowing specific input of elements
#'
#' @return updated named list with your data assigned to Stan model parameters
#' @export

hmde_assign_data <- function(model_template, data = NULL,...){
  if(!model_template$model %in% hmde_model_name()){
    stop("Model name not recognised. Run hmde_model_name() to see available models.")
  }

  if(!is.null(data)){ # Use provided tibble
    user_fields <- names(data)

  } else { # Grab user expressions from individual list items and extract data
    user_code <- rlang::enquos(..., .check_assign = TRUE)
    user_fields <- names(user_code)
    # Evaluate the RHS of expressions (the values)
    data <- purrr::map(user_code,
                       ~rlang::eval_tidy(.x, env = rlang::caller_env())
    )
  }

  # Grab the names
  model_fields <- names(model_template)

  # Check user data has required names
  if(grepl("multi", model_template$model)){ # Multi-individual with ind_id vec
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

  for(i in model_fields){ # Iterate through required fields and fill them
    if(i %in% user_fields){
      model_template <- purrr::list_modify(model_template, !!!data[i])
    } else {
      model_template[[i]] <- switch(
        i,
        n_obs = length(data$y_obs),
        n_ind = length(unique(data$ind_id)),
        y_bar = mean(data$y_obs),
        model = model_template$model
      )
    }

    if(is.null(model_template[[i]])){ #Report missing data
      stop(paste("Improper data structure: Data missing:", i))
    }
  }

  #Check lengths for y_obs, obs_index, time, ind_id
  vec_lengths <- c(
    model_template$n_obs,
    length(model_template$y_obs),
    length(model_template$obs_index),
    length(model_template$time)
  )
  if(grepl("multi", model_template$model)){ # Multi-individual with ind_id vector
    vec_lengths[5] <- length(model_template$ind_id)

    #Check number ind ID values
    ind_id_lengths <- c(model_template$n_ind,length(unique(data$ind_id)))
    if(length(unique(ind_id_lengths))!=1){
      stop("Different values for n_ind and number of unique entries in ind_id.")
    }
  }

  if(length(unique(vec_lengths))!=1){
    stop("Improper data structure: Different lengths of data vectors.")
  }

  return(model_template)
}
