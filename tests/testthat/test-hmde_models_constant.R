#Testing for constant model
test_that("Model structures: Constant", {
  # Single individual
  single_model <- hmde_model("constant_single_ind")
  expect_named(single_model, c("n_obs", "y_obs",
                               "obs_index", "time",
                               "prior_pars_ind_beta",
                               "prior_pars_global_error_sigma",
                               "model"))
  expect_type(single_model, "list")
  expect_visible(single_model)

  #Multiple individuals
  multi_model <- hmde_model("constant_multi_ind")
  expect_named(multi_model, c("n_obs", "n_ind", "y_obs",
                              "obs_index", "time", "ind_id",
                              "prior_pars_pop_log_beta_mu",
                              "prior_pars_pop_log_beta_sigma",
                              "prior_pars_global_error_sigma",
                              "model"))
  expect_type(multi_model, "list")
  expect_visible(multi_model)
})

test_that("Execution: Constant single individual", {
  model_name <- "constant"
  par_names <- "ind_beta"

  hmde_test_single_individual(model_name, par_names)
})

test_that("Execution: Constant multiple individuals", {
  model_name <- "constant"

  data <- readRDS(test_path("fixtures", "constant",
                            "constant_data_multi_ind.rds"))

  #Dimension is:
  est_dim <- data$n_ind +     #Initial condition
    data$n_pars * data$n_ind + #Individual parameters
    data$n_pars * 2  +         #Population parameters
    1 +                        #Global error
    data$n_obs +               #y_ij
    6 +                        #checks for priors
    1                          #lp__

  hmde_test_multi_individual(model_name, data, est_dim)
})
