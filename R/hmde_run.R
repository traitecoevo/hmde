#' Run chosen pre-built model in Stan
#'
#' @param data_template hmde_data_template class object
#' @param ... additional arguments passed to rstan::sampling
#'
#' @return Stanfit S4 object
#'
#' @examples
#' # basic usage of hmde_run
#' hmde_data_template("constant_single_ind",
#'                    obs_data = Trout_Size_Data[1:4,]) |>
#'   hmde_run(chains = 1, iter = 1000,
#'            verbose = FALSE, show_messages = FALSE)
#'
#' @export

hmde_run <- function(data_template, ...){
  #Verify data_template type
  if(!inherits(data_template, "hmde_data_template")){
    stop("Model template not required class hmde_data_template.")
  }

  #Check for model
  if(!model_name(data_template) %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  #Construct list object for model
  data_list <- list(model_name = model_name(data_template))
  data_list <- append(data_list, obs_data(data_template))
  data_list <- append(data_list, prior_pars(data_template))

  #Verify list names
  list_names <- hmde_stan_names(data_list$model_name)
  for(i in list_names){
    if(!i %in% names(data_list)){
      stop(paste0("Data element missing: ", i))
    }
  }

  #Run model
  out <- hmde_run_model(data_list, ...)

  return(out)
}

#-----------------------------------------------------------------------------#
# Helper functions

#' Returns a vector of names for the list passed to stan
#' @keywords internal
#' @noRd
hmde_stan_names <- function(model_name){
  out <- switch(model_name,
                constant_single_ind = c("n_obs", "y_obs", "obs_index", "time",
                                        "prior_pars_ind_beta",
                                        "prior_pars_global_error_sigma"),
                constant_multi_ind = c("n_obs", "n_ind", "y_obs", "obs_index",
                                       "time", "ind_id",
                                       "prior_pars_pop_log_beta_mean",
                                       "prior_pars_pop_log_beta_sd",
                                       "prior_pars_global_error_sigma"),
                canham_single_ind = c("n_obs", "y_obs", "obs_index", "time",
                                      "prior_pars_ind_max_growth",
                                      "prior_pars_ind_size_at_max_growth",
                                      "prior_pars_ind_k",
                                      "prior_pars_global_error_sigma"),
                canham_multi_ind = c("n_obs", "n_ind", "y_obs", "obs_index",
                                     "time", "ind_id",
                                     "prior_pars_pop_log_max_growth_mean",
                                     "prior_pars_pop_log_max_growth_sd",
                                     "prior_pars_pop_log_size_at_max_growth_mean",
                                     "prior_pars_pop_log_size_at_max_growth_sd",
                                     "prior_pars_pop_log_k_mean",
                                     "prior_pars_pop_log_k_sd",
                                     "prior_pars_global_error_sigma"),
                vb_single_ind = c("n_obs", "y_obs", "obs_index", "time", "y_bar",
                                  "prior_pars_ind_max_size_sd_only",
                                  "prior_pars_ind_growth_rate",
                                  "prior_pars_global_error_sigma"),
                vb_multi_ind = c("n_obs", "n_ind", "y_obs", "obs_index",
                                 "time", "ind_id", "y_bar",
                                 "prior_pars_pop_log_max_size_mean_sd_only",
                                 "prior_pars_pop_log_max_size_sd",
                                 "prior_pars_pop_log_growth_rate_mean",
                                 "prior_pars_pop_log_growth_rate_sd",
                                 "prior_pars_global_error_sigma"),
                affine_single_ind = c("step_size", "n_obs", "y_obs", "obs_index",
                                      "time", "y_bar", "int_method",
                                      "prior_pars_ind_const",
                                      "prior_pars_ind_beta_1"),
                zeide_single_ind = c("n_obs", "y_obs", "obs_index", "time",
                                     "prior_pars_ind_a",
                                     "prior_pars_ind_b",
                                     "prior_pars_ind_c",
                                     "prior_pars_global_error_sigma"),
                zeide_multi_ind = c("n_obs", "n_ind", "y_obs", "obs_index",
                                    "time", "ind_id",
                                    "prior_pars_pop_log_a_mean",
                                    "prior_pars_pop_log_a_sd",
                                    "prior_pars_pop_log_b_mean",
                                    "prior_pars_pop_log_b_sd",
                                    "prior_pars_pop_log_c_mean",
                                    "prior_pars_pop_log_c_sd",
                                    "prior_pars_global_error_sigma"))

  return(out)
}

#' Run selected model through stan sampling
#' @keywords internal
#' @noRd
hmde_run_model <- function(data_list, ...) {
  # Detect model
  out <- switch(data_list$model_name,
         constant_single_ind = rstan::sampling(stanmodels$constant_single_ind, data = data_list, ...),
         constant_multi_ind = rstan::sampling(stanmodels$constant_multi_ind, data = data_list, ...),
         canham_single_ind = rstan::sampling(stanmodels$canham_single_ind, data = data_list, ...),
         canham_multi_ind = rstan::sampling(stanmodels$canham_multi_ind, data = data_list, ...),
         vb_single_ind = rstan::sampling(stanmodels$vb_single_ind, data = data_list, ...),
         vb_multi_ind = rstan::sampling(stanmodels$vb_multi_ind, data = data_list, ...),
         affine_single_ind = rstan::sampling(stanmodels$affine_single_ind, data = data_list, ...),
         zeide_single_ind = rstan::sampling(stanmodels$zeide_single_ind, data = data_list, ...),
         zeide_multi_ind = rstan::sampling(stanmodels$zeide_multi_ind, data = data_list, ...))

  return(out)
}
