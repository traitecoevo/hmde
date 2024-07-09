#Testing for canham model
test_that("Model structures: canham", {
  # Single individual
  single_model <- rmot_model("canham_single_ind")
  expect_named(single_model, c("step_size", "n_obs", "y_obs",
                               "obs_index", "time", "y_0_obs",
                               "model"))
  expect_type(single_model, "list")
  expect_visible(single_model)

  #Multiple individuals
  multi_model <- rmot_model("canham_multi_ind")
  expect_named(multi_model, c("step_size", "n_obs", "n_ind", "y_obs",
                              "obs_index", "time", "ind_id", "y_0_obs",
                              "model"))
  expect_type(multi_model, "list")
  expect_visible(multi_model)
})

test_that("Execution: canham single individual", {
  model_name <- "canham"
  par_names <- c("ind_max_growth", "ind_diameter_at_max_growth", "ind_k")

  rmot_test_single_individual(model_name, par_names)
})

test_that("Test New model", {
  model_name <- "canham"

  data <- readRDS(test_path("fixtures", "canham",
    "canham_data_multi_ind.rds"))
  start_points = c(1, which(diff(data$ind_id) == 1) + 1)
  data$ind_id[start_points]

  end_points = c(which(diff(data$ind_id) == 1), length(data$ind_id))
  data$ind[end_points]
  start_stop_points = list()
  for (i in 1:length(end_points)) {
    start_stop_points = c(start_stop_points, list(array(c(start_points[i], end_points[i]))))
  }
  library(cmdstanr)
  data2 = data[1:9]
  data2$group_start_stop_idx = array(0,dim = c(length(end_points), 2))
  for (i in 1:length(end_points)) {
    data2$group_start_stop_idx[i,] = c(start_points[i], end_points[i])
  }
  mod = cmdstanr::cmdstan_model("./inst/stan/canham_multi_ind.stan")
  fit = mod$sample(data = data2, chains = 4, parallel_chains = 4)
})

test_that("Execution: canham multiple individuals", {
  model_name <- "canham"

  data <- readRDS(test_path("fixtures", "canham",
                            "canham_data_multi_ind.rds"))
  start_points = c(1, which(diff(data$ind_id) == 1) + 1)
  data$ind_id[start_points]

  end_points = c(which(diff(data$ind_id) == 1), length(data$ind_id))
  data$ind[end_points]
  start_stop_points = list()
  for (i in 1:length(end_points)) {
    start_stop_points = c(start_stop_points, list(array(c(start_points[i], end_points[i]))))
  }
  data$start_stop_points = start_stop_points
  library(cmdstanr)
  data2 = data[1:9]
  data2$group_start_stop_idx = array(unlist(start_stop_points),dim = c(length(end_points), 2))
  for (i in 1:length(end_points)) {
    data2$group_start_stop_idx[i,] = c(start_points[i], end_points[i])
  }
  mod = cmdstanr::cmdstan_model("./inst/stan/canham_multi_ind.stan")
  fit = mod$sample(data = data2, chains = 4, parallel_chains = 4)
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
