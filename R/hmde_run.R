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

hmde_run <- function(data_template, ...) {
  if(!inherits(data_template, "hmde_data_template")){
    stop("Model template not required class hmde_data_template.")
  }

  #Check for model
  if(!data_template$model %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  # Detect model
  out <- switch(data_template$model,
         constant_single_ind = rstan::sampling(stanmodels$constant_single_ind, data = data_template, ...),
         constant_multi_ind = rstan::sampling(stanmodels$constant_multi_ind, data = data_template, ...),
         canham_single_ind = rstan::sampling(stanmodels$canham_single_ind, data = data_template, ...),
         canham_multi_ind = rstan::sampling(stanmodels$canham_multi_ind, data = data_template, ...),
         vb_single_ind = rstan::sampling(stanmodels$vb_single_ind, data = data_template, ...),
         vb_multi_ind = rstan::sampling(stanmodels$vb_multi_ind, data = data_template, ...),
         affine_single_ind = rstan::sampling(stanmodels$affine_single_ind, data = data_template, ...))

  return(out)
}
