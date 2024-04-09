#Testing for canham model
test_that("Model structures: canham", {
  # Single individual
  expect_named(rmot_model("canham_single_ind"))
  expect_type(rmot_model("canham_single_ind"), "list")
  expect_visible(rmot_model("canham_single_ind"))

  #Multiple individuals
  expect_named(rmot_model("canham_multi_ind"))
  expect_type(rmot_model("canham_multi_ind"), "list")
  expect_visible(rmot_model("canham_multi_ind"))
})

test_that("Execution: canham single individual", {
  model_name <- "canham"
  par_names <- c("ind_max_growth", "ind_diameter_at_max_growth", "ind_k")

  rmot_test_single_individual(model_name, par_names)
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

  rmot_test_multi_individual(model_name, data, est_dim)
})
