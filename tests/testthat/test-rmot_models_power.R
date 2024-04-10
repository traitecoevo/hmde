#Testing for power model
test_that("Model structures: power", {
  # Single individual
  single_model <- rmot_model("power_single_ind")
  expect_named(single_model, c("step_size", "n_obs", "y_obs", "obs_index",
                               "time", "y_bar", "y_0_obs", "model"))
  expect_type(single_model, "list")
  expect_visible(single_model)

  #Multiple individuals
  multi_model <- rmot_model("power_multi_ind")
  expect_named(multi_model, c("step_size", "n_obs", "n_ind", "y_obs",
                              "obs_index", "time", "ind_id", "y_bar",
                              "y_0_obs", "model"))
  expect_type(multi_model, "list")
  expect_visible(multi_model)
})

test_that("Execution: power single individual", {
  model_name <- "power"
  par_names <- c("ind_coeff", "ind_power")
  data <- readRDS(test_path("fixtures", "power",
                            "power_data_single_ind.rds"))

  suppressWarnings( #Suppresses stan warnings
    single_ind_test <- rmot_model(paste0(model_name, "_single_ind")) |>
      rmot_assign_data(step_size = data$step_size,
                       n_obs = data$n_obs, #integer
                       y_obs = data$y_obs,
                       obs_index = data$obs_index, #vector length N_obs
                       time = data$time, #Vector length N_obs
                       y_bar = data$single_true_data$DE_pars$y_bar, #Real
                       y_0_obs = data$y_0_obs #vector length N_ind
      ) |>
      rmot_run(chains = 1, iter = 1000, verbose = FALSE, show_messages = FALSE)
  )

  # Extract samples and check if parameter estimates are reasonable.
  ind_samples <- rstan::extract(single_ind_test, permuted = TRUE,
                                inc_warmup = FALSE)
  par_ests <- c()
  for(i in seq_along(par_names)){
    par_ests[i] <- mean(ind_samples[[par_names[i]]])
  }

  initial_condition <- mean(ind_samples$ind_y_0)
  expect_equal(par_ests,
               as.numeric(data$single_true_data$DE_pars[c(1,2)]),
               tolerance = 1e-1)
  expect_equal(initial_condition,
               as.numeric(data$single_true_data$initial_conditions$y_0),
               tolerance = 1e-1)

  # hecks for output existence and type
  expect_visible(single_ind_test)
  expect_s4_class(single_ind_test, "stanfit")
})

test_that("Execution: power multiple individuals", {
  model_name <- "power"

  data <- readRDS(test_path("fixtures", "power",
                            "power_data_multi_ind.rds"))

  #Dimension is:
  est_dim <- data$n_ind +           #Initial condition
    (data$n_pars-1) * data$n_ind +  #Individual parameters, -1 as y_bar not estimated
    (data$n_pars-1) * 2 +           #Population parameters
    1 +                             #Global error
    data$n_obs +                    #y_ij
    data$n_obs +                    #Delta y_ij
    (data$n_pars-1) +               #Pars temp vector
    1                               #lp__

  # Test multi-individual
  suppressWarnings( #Suppresses stan warnings
    multi_ind_test <- rmot_model(paste0(model_name, "_multi_ind")) |>
      rmot_assign_data(step_size = data$step_size, #real
                       n_obs = data$n_obs, #integer
                       n_ind = data$n_ind, #integer
                       y_obs = data$y_obs, #vector length N_obs
                       obs_index = data$obs_index, #vector length N_obs
                       time = data$time, #Vector length N_obs
                       ind_id = data$ind_id, #Vector length N_obs
                       y_bar = data$multi_true_data$initial_conditions$y_bar, #Real
                       y_0_obs = data$y_0_obs #vector length N_ind
      ) |>
      rmot_run(chains = 2, iter = 100, verbose = FALSE, show_messages = FALSE)
  )

  # Extract samples
  multi_samples <- rstan::extract(multi_ind_test, permuted = FALSE, inc_warmup = TRUE)
  expect_equal(dim(multi_samples), c(100, 2, est_dim))

  expect_visible(multi_ind_test)
  expect_s4_class(multi_ind_test, "stanfit")
})
