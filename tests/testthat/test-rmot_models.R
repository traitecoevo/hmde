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

  # Canham models
  # Single individual
  expect_named(rmot_model("canham_single_ind"))
  expect_type(rmot_model("canham_single_ind"), "list")
  expect_visible(rmot_model("canham_single_ind"))
  #Multiple individuals
  expect_named(rmot_model("canham_multi_ind"))
  expect_type(rmot_model("canham_multi_ind"), "list")
  expect_visible(rmot_model("canham_multi_ind"))

  # VB models
  # Single individual
  expect_named(rmot_model("vb_single_ind"))
  expect_type(rmot_model("vb_single_ind"), "list")
  expect_visible(rmot_model("vb_single_ind"))
  #Multiple individuals
  expect_named(rmot_model("vb_multi_ind"))
  expect_type(rmot_model("vb_multi_ind"), "list")
  expect_visible(rmot_model("vb_multi_ind"))
})

test_that("Execution and output: Linear", {
  lm_data <- readRDS(test_path("fixtures", "linear", "lm_data.rds"))
  lm_baseline_output <- readRDS(test_path("fixtures", "linear", "lm_baseline_output.rds"))

  # Test linear model
  set.seed(2024)
  suppressWarnings( #Suppresses stan warnings
    lm_test <- rmot_model("linear") |>
      rmot_assign_data(X = lm_data$X,
                       Y = lm_data$Y,
                       N = nrow(lm_data)) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
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
  set.seed(2024)

  suppressWarnings( #Suppresses stan warnings
    constant_single_ind_test <- rmot_model("constant_single_ind") |>
      rmot_assign_data(n_obs = const_data$n_obs, #integer
                       y_obs = const_data$y_obs,
                       obs_index = const_data$obs_index, #vector length N_obs
                       time = const_data$time, #Vector length N_obs
                       y_0_obs = const_data$y_0_obs #vector length N_ind
      ) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
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
  set.seed(2024)
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
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  expect_equal(rstan::summary(constant_multi_ind_test)$summary,
               const_multi_ind_baseline_output, tolerance = 1e-5)
  expect_visible(constant_multi_ind_test)
  expect_s4_class(constant_multi_ind_test, "stanfit")
})

test_that("Execution: Canham single individual", {
  canham_data <- readRDS(test_path("fixtures", "canham",
                                  "canham_data_single_ind.rds"))
  canham_single_ind_baseline_output <- readRDS(test_path("fixtures", "canham",
                                                        "canham_baseline_output_single_ind.rds"))

  # Test constant single individual
  set.seed(2024)

  suppressWarnings( #Suppresses stan warnings
    canham_single_ind_test <- rmot_model("canham_single_ind") |>
      rmot_assign_data(step_size = 1.0, #real
                       n_obs = canham_data$n_obs, #integer
                       y_obs = canham_data$y_obs,
                       obs_index = canham_data$obs_index, #vector length N_obs
                       time = canham_data$time, #Vector length N_obs
                       y_0_obs = canham_data$y_0_obs #vector length N_ind
      ) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  expect_equal(rstan::summary(canham_single_ind_test)$summary,
               canham_single_ind_baseline_output, tolerance = 1e-5)
  expect_visible(canham_single_ind_test)
  expect_s4_class(canham_single_ind_test, "stanfit")
})

test_that("Execution: Canham multiple individuals", {
  canham_data <- readRDS(test_path("fixtures", "canham",
                                  "canham_data_multi_ind.rds"))
  canham_multi_ind_baseline_output <- readRDS(test_path("fixtures", "canham",
                                                       "canham_baseline_output_multi_ind.rds"))

  # Test constant multi-individual
  set.seed(2024)
  suppressWarnings( #Suppresses stan warnings
    canham_multi_ind_test <- rmot_model("canham_multi_ind") |>
      rmot_assign_data(step_size = 1.0, #real
                       n_obs = canham_data$n_obs, #integer
                       n_ind = canham_data$n_ind, #integer
                       y_obs = canham_data$y_obs, #vector length N_obs
                       obs_index = canham_data$obs_index, #vector length N_obs
                       time = canham_data$time, #Vector length N_obs
                       ind_id = canham_data$ind_id, #Vector length N_obs
                       y_0_obs = canham_data$y_0_obs #vector length N_ind
      ) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  expect_equal(rstan::summary(canham_multi_ind_test)$summary,
               canham_multi_ind_baseline_output, tolerance = 1e-5)
  expect_visible(canham_multi_ind_test)
  expect_s4_class(canham_multi_ind_test, "stanfit")
})

test_that("Execution: von Bertalanffy single individual", {
  vb_data <- readRDS(test_path("fixtures", "vb",
                                   "vb_data_single_ind.rds"))
  vb_single_ind_baseline_output <- readRDS(test_path("fixtures", "vb",
                                                         "vb_baseline_output_single_ind.rds"))

  # Test constant single individual
  set.seed(2024)

  suppressWarnings( #Suppresses stan warnings
    vb_single_ind_test <- rmot_model("vb_single_ind") |>
      rmot_assign_data(step_size = 1.0, #real
                       n_obs = vb_data$n_obs, #integer
                       y_obs = vb_data$y_obs,
                       obs_index = vb_data$obs_index, #vector length N_obs
                       time = vb_data$time, #Vector length N_obs
                       y_0_obs = vb_data$y_0_obs #vector length N_ind
      ) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  expect_equal(rstan::summary(vb_single_ind_test)$summary,
               vb_single_ind_baseline_output, tolerance = 1e-5)
  expect_visible(vb_single_ind_test)
  expect_s4_class(vb_single_ind_test, "stanfit")
})

test_that("Execution: von Bertalanffy multiple individuals", {
  vb_data <- readRDS(test_path("fixtures", "vb",
                                   "vb_data_multi_ind.rds"))
  vb_multi_ind_baseline_output <- readRDS(test_path("fixtures", "vb",
                                                    "vb_baseline_output_multi_ind.rds"))

  # Test vb multi-individual
  set.seed(2024)
  suppressWarnings( #Suppresses stan warnings
    vb_multi_ind_test <- rmot_model("vb_multi_ind") |>
      rmot_assign_data(step_size = 1.0, #real
                       n_obs = vb_data$n_obs, #integer
                       n_ind = vb_data$n_ind, #integer
                       y_obs = vb_data$y_obs, #vector length N_obs
                       obs_index = vb_data$obs_index, #vector length N_obs
                       time = vb_data$time, #Vector length N_obs
                       ind_id = vb_data$ind_id, #Vector length N_obs
                       y_0_obs = vb_data$y_0_obs #vector length N_ind
      ) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  expect_equal(rstan::summary(vb_multi_ind_test)$summary,
               vb_multi_ind_baseline_output, tolerance = 1e-5)
  expect_visible(vb_multi_ind_test)
  expect_s4_class(vb_multi_ind_test, "stanfit")
})
