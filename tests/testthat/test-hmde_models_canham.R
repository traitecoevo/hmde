#Testing for canham model
test_that("Model structures: canham", {
  # Single individual
  single_model <- hmde_model("canham_single_ind")
  expect_named(single_model, c("step_size", "n_obs", "y_obs",
                               "obs_index", "time", "y_0_obs",
                               "model"))
  expect_type(single_model, "list")
  expect_visible(single_model)

  #Multiple individuals
  multi_model <- hmde_model("canham_multi_ind")
  expect_named(multi_model, c("step_size", "n_obs", "n_ind", "y_obs",
                              "obs_index", "time", "ind_id", "y_0_obs",
                              "model"))
  expect_type(multi_model, "list")
  expect_visible(multi_model)
})

test_that("Execution: canham single individual", {
  model_name <- "canham"
  par_names <- c("ind_max_growth", "ind_size_at_max_growth", "ind_k")

  hmde_test_single_individual(model_name, par_names)
})

test_that("Execution: canham multiple individuals", {
  model_name <- "canham"

  data <- readRDS(test_path("fixtures", "canham",
                            "canham_data_multi_ind.rds"))

  #Dimension is:
  est_dim <- data$n_ind +      #Initial condition
    data$n_pars * data$n_ind + #Individual parameters
    data$n_pars * 2  +         #Population parameters
    1 +                        #Global error
    data$n_obs +               #y_ij
    data$n_obs +               #Delta y_ij
    data$n_pars +              #pars vector
    1                          #lp__

  hmde_test_multi_individual(model_name, data, est_dim)
})
