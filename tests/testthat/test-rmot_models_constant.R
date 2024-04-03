#Testing for constant model
test_that("Model structures: Constant", {
  # Single individual
  expect_named(rmot_model("constant_single_ind"))
  expect_type(rmot_model("constant_single_ind"), "list")
  expect_visible(rmot_model("constant_single_ind"))
  #Multiple individuals
  expect_named(rmot_model("constant_multi_ind"))
  expect_type(rmot_model("constant_multi_ind"), "list")
  expect_visible(rmot_model("constant_multi_ind"))
})

test_that("Execution: Constant single individual", {
  model_name <- "constant"
  par_names <- "ind_beta"

  rmot_test_single_individual(model_name, par_names)
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
    data$n_obs +               #Delta y_ij
    1                          #lp__

  rmot_test_multi_individual(model_name, data, est_dim)
})
