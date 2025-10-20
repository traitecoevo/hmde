#' Calculate Rhat statistics for a hmde_fit object
#'
#' @param fit hmde_fit fitted model object, output of hmde_run
#'
#' @return named vector of Rhat values
#'
#' @examples
#' # basic usage of hmde_extract_Rhat
#' hmde_model("constant_single_ind") |>
#'   hmde_assign_data(Trout_Size_Data)|>
#'   hmde_run(chains = 2, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE) |>
#'   hmde_extract_Rhat()
#'
#' @export
#'

hmde_extract_Rhat <- function(fit){
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

  #We want to exclude the check_prior_pars_ generated quantities from Rhat
  temp_samples <- rstan::extract(fit, permuted = TRUE,
                                 inc_warmup = FALSE)
  temp_sample_names <- names(temp_samples)

  #Construct a vector that has repeated sample names for the appropriate dimension
  sample_name_vec <- c()
  for(i in 1:length(temp_sample_names)){
    times <- if(is.na(ncol(temp_samples[[i]]))) 1 else ncol(temp_samples[[i]])
    sample_name_vec <- c(sample_name_vec,
                         rep(temp_sample_names[i], times = times))
  }
  sample_par_index <- which(!grepl("check_prior_pars_", sample_name_vec))

  samples <- rstan::extract(fit, permuted = FALSE, inc_warmup = TRUE)

  R_hat_vec <- c()
  for(i in 1:length(sample_par_index)){
    temp_names <- c(names(R_hat_vec),
                    sample_name_vec[sample_par_index[i]])
    temp_vals <- c(as.numeric(R_hat_vec),
                   Rhat(samples[,,sample_par_index[i]]))
    R_hat_vec <- setNames(temp_vals,temp_names)
  }

  return(R_hat_vec)
}
