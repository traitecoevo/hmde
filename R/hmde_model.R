#' helper function for hmde_data_template that provides data structure for model,
#' Also used to guide the user on the required structure for the model they want.
#'
#' @param model model name character string
#'
#' @examples
#' # basic usage of hmde_model
#' hmde_model("constant_single_ind")
#'
#' @return hmde_data_template class that suits Stan model input
#' @export

hmde_model <- function(model=NULL){

  if(!model %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  output <- switch(model,
                   constant_single_ind = hmde_const_single_ind(),
                   constant_multi_ind = hmde_const_multi_ind(),
                   canham_single_ind = hmde_canham_single_ind(),
                   canham_multi_ind = hmde_canham_multi_ind(),
                   vb_single_ind = hmde_vb_single_ind(),
                   vb_multi_ind = hmde_vb_multi_ind(),
                   affine_single_ind = hmde_affine_single_ind())

  return(output)
}

#' Data configuration template for constant growth single individual model
#' @keywords internal
#' @noRd

hmde_const_single_ind <- function(){
  new("hmde_data_template",
    model_name = "constant_single_ind",
    model_level = "single_ind",
    obs_data = list(
      n_obs = NA_integer_,
      y_obs = NA_real_,
      obs_index = NA_integer_,
      time = NA_real_
    ),
    prior_pars = list(
      prior_pars_ind_beta = c(0, 2),
      prior_pars_global_error_sigma = c(0, 2)
    ),
    par_names = hmde_model_pars("constant_single_ind")
  )
}

#' Data configuration template for constant growth single species model
#' @keywords internal
#' @noRd

hmde_const_multi_ind <- function(){
  new("hmde_data_template",
    model_name = "constant_multi_ind",
    model_level = "multi_ind",
    obs_data = list(
      n_obs = NA_integer_,
      n_ind = NA_integer_,
      y_obs = NA_real_,
      obs_index = NA_integer_,
      time = NA_real_,
      ind_id = NA_integer_
    ),
    prior_pars = list(
      prior_pars_pop_log_beta_mean = c(0,2),
      prior_pars_pop_log_beta_sd = c(0,2),
      prior_pars_global_error_sigma = c(0,2)
    ),
    par_names = hmde_model_pars("constant_multi_ind")
  )
}

#' Data configuration template for Canham growth single individual model
#' @keywords internal
#' @noRd

hmde_canham_single_ind <- function(){
  new("hmde_data_template",
    model_name = "canham_single_ind",
    model_level = "single_ind",
    obs_data = list(
      n_obs = NA_integer_,
      y_obs = NA_real_,
      obs_index = NA_integer_,
      time = NA_real_
    ),
    prior_pars = list(
      prior_pars_ind_max_growth = c(0,2),
      prior_pars_ind_size_at_max_growth = c(0,2),
      prior_pars_ind_k = c(0,2),
      prior_pars_global_error_sigma = c(0,2)
    ),
    par_names = hmde_model_pars("canham_single_ind")
  )
}

#' Data configuration template for Canham growth single species model
#' @keywords internal
#' @noRd

hmde_canham_multi_ind <- function(){
  new("hmde_data_template",
    model_name = "canham_multi_ind",
    model_level = "multi_ind",
    obs_data = list(
      n_obs = NA_integer_,
      n_ind = NA_integer_,
      y_obs = NA_real_,
      obs_index = NA_integer_,
      time = NA_real_,
      ind_id = NA_integer_
    ),
    prior_pars = list(
      prior_pars_pop_log_max_growth_mean = c(0,2),
      prior_pars_pop_log_max_growth_sd = c(0,2),
      prior_pars_pop_log_size_at_max_growth_mean = c(0,2),
      prior_pars_pop_log_size_at_max_growth_sd = c(0,2),
      prior_pars_pop_log_k_mean = c(0,2),
      prior_pars_pop_log_k_sd = c(0,2),
      prior_pars_global_error_sigma = c(0,2)
    ),
    par_names = hmde_model_pars("canham_multi_ind")
  )
}


#' Data configuration template for von Bertalanffy growth single individual model
#' @keywords internal
#' @noRd

hmde_vb_single_ind <- function(){
  new("hmde_data_template",
    model_name = "vb_single_ind",
    model_level = "single_ind",
    obs_data = list(
      n_obs = NA_integer_,
      y_obs = NA_real_,
      obs_index = NA_integer_,
      time = NA_real_,
      y_bar = NA_real_
    ),
    prior_pars = list(
      prior_pars_ind_max_size_sd_only = 2,
      prior_pars_ind_growth_rate = c(0,2),
      prior_pars_global_error_sigma = c(0,2)
    ),
    par_names = hmde_model_pars("vb_single_ind")
  )
}

#' Data configuration template for von Bertalanffy growth single species model
#' @keywords internal
#' @noRd

hmde_vb_multi_ind <- function(){
  new("hmde_data_template",
    model_name = "vb_multi_ind",
    model_level = "multi_ind",
    obs_data = list(
      n_obs = NA_integer_,
      n_ind = NA_integer_,
      y_obs = NA_real_,
      obs_index = NA_integer_,
      time = NA_real_,
      ind_id = NA_integer_,
      y_bar = NA_real_
    ),
    prior_pars = list(
      prior_pars_pop_log_max_size_mean_sd_only = 2,
      prior_pars_pop_log_max_size_sd = c(0,2),
      prior_pars_pop_log_growth_rate_mean = c(0,2),
      prior_pars_pop_log_growth_rate_sd = c(0,2),
      prior_pars_global_error_sigma = c(0,2)
    ),
    par_names = hmde_model_pars("vb_multi_ind")
  )
}

#' Data configuration template for affine growth single individual model
#' @keywords internal
#' @noRd
#'
hmde_affine_single_ind <- function(){
  new("hmde_data_template",
    model_name = "affine_single_ind",
    model_level = "single_ind",
    obs_data = list(
      n_obs = NA_integer_,
      y_obs = NA_real_,
      obs_index = NA_integer_,
      time = NA_real_,
      step_size = NA_real_,
      int_method = NA_integer_,
      y_bar = NA_real_
    ),
    prior_pars = list(
      prior_pars_ind_const = c(1,2),
      prior_pars_ind_beta_1 = c(0,2)
    ),
    par_names = hmde_model_pars("affine_single_ind")
  )
}
