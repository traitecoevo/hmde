test_that("Model structures", {
  expect_named(rmot_model("linear"))
  expect_type(rmot_model("linear"), "list")
  expect_visible(rmot_model("linear"))

  # Constant models
  # Single individual
  expect_named(rmot_model("constant_single_ind"))
  expect_type(rmot_model("constant_single_ind"), "list")
  expect_visible(rmot_model("constant_single_ind"))
  #Multiple individuals
  expect_named(rmot_model("constant_multi_ind"))
  expect_type(rmot_model("constant_multi_ind"), "list")
  expect_visible(rmot_model("constant_multi_ind"))
})

test_that("Execution and output: Linear", {
  lm_data <- readRDS(test_path("fixtures", "linear", "lm_data.rds"))
  lm_baseline_output <- readRDS(test_path("fixtures", "linear", "lm_baseline_output.rds"))

  # Test linear model
  suppressWarnings( #Suppresses stan warnings
    lm_test <- rmot_model("linear") |>
      rmot_assign_data(X = lm_data$X,
                       Y = lm_data$Y,
                       N = nrow(lm_data)) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE,
      seed = 1)
  )

  expect_equal(rstan::summary(lm_test)$summary, lm_baseline_output, tolerance = 1e-5)
  expect_visible(lm_test)
  expect_s4_class(lm_test, "stanfit")
})

test_that("Execution: Constant single individual", {
  const_data <- readRDS(test_path("fixtures", "constant",
                                   "constant_data_single_ind.rds"))
  const_single_ind_baseline_output <- readRDS(test_path("fixtures", "constant",
                                                        "constant_baseline_output_single_ind.rds"))

   # Test constant single individual
  suppressWarnings( #Suppresses stan warnings
    constant_single_ind_test <- rmot_model("constant_single_ind") |>
      rmot_assign_data(n_obs = const_data$n_obs, #integer
                       y_obs = const_data$y_obs,
                       obs_index = const_data$obs_index, #vector length N_obs
                       time = const_data$time, #Vector length N_obs
                       y_0_obs = const_data$y_0_obs #vector length N_ind
      ) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE,
      seed = 1)
  )

  expect_equal(rstan::summary(constant_single_ind_test)$summary,
               const_single_ind_baseline_output, tolerance = 1e-5)
  expect_visible(constant_single_ind_test)
  expect_s4_class(constant_single_ind_test, "stanfit")

})

test_that("Execution: Constant multiple individuals", {
  const_data <- readRDS(test_path("fixtures", "constant",
                                  "constant_data_multi_ind.rds"))
  const_multi_ind_baseline_output <- readRDS(test_path("fixtures", "constant",
                                                       "constant_baseline_output_multi_ind.rds"))

  # Test constant multi-individual
  suppressWarnings( #Suppresses stan warnings
    constant_multi_ind_test <- rmot_model("constant_multi_ind") |>
      rmot_assign_data(n_obs = const_data$n_obs, #integer
                       n_ind = const_data$n_ind, #integer
                       y_obs = const_data$y_obs, #vector length N_obs
                       obs_index = const_data$obs_index, #vector length N_obs
                       time = const_data$time, #Vector length N_obs
                       ind_id = const_data$ind_id, #Vector length N_obs
                       y_0_obs = const_data$y_0_obs #vector length N_ind
      ) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE,
      seed = 1)
  )
  rstan::extract(constant_multi_ind_test, permuted = FALSE, inc_warmup = FALSE)
  expect_equal(rstan::summary(constant_multi_ind_test)$summary,
               const_multi_ind_baseline_output, tolerance = 1e-5)
  expect_visible(constant_multi_ind_test)
  expect_s4_class(constant_multi_ind_test, "stanfit")
})
