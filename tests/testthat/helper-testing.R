rmot_test_model_functions <- function(model_name){
  single_ind_name <- paste0(model_name, "_single_ind")
  multi_ind_name <- paste0(model_name, "_multi_ind")

  # Single individual
  expect_named(rmot_model(single_ind_name))
  expect_type(rmot_model(single_ind_name), "list")
  expect_visible(rmot_model(single_ind_name))
  #Multiple individuals
  expect_named(rmot_model(multi_ind_name))
  expect_type(rmot_model(multi_ind_name), "list")
  expect_visible(rmot_model(multi_ind_name))
}

rmot_test_single_individual <- function(model_name,
                                        par_names){
  data <- readRDS(test_path("fixtures", model_name,
                            paste0(model_name, "_data_single_ind.rds")))

  if(! is.null(data$step_size)){
    # Test single individual
    suppressWarnings( #Suppresses stan warnings
      single_ind_test <- rmot_model(paste0(model_name, "_single_ind")) |>
        rmot_assign_data(step_size = data$step_size,
                         n_obs = data$n_obs, #integer
                         y_obs = data$y_obs,
                         obs_index = data$obs_index, #vector length N_obs
                         time = data$time, #Vector length N_obs
                         y_0_obs = data$y_0_obs #vector length N_ind
        ) |>
        rmot_run(chains = 1, iter = 1000, verbose = FALSE, show_messages = FALSE)
    )
  } else {
    # Test single individual
    suppressWarnings( #Suppresses stan warnings
      single_ind_test <- rmot_model(paste0(model_name, "_single_ind")) |>
        rmot_assign_data(n_obs = data$n_obs, #integer
                         y_obs = data$y_obs,
                         obs_index = data$obs_index, #vector length N_obs
                         time = data$time, #Vector length N_obs
                         y_0_obs = data$y_0_obs #vector length N_ind
        ) |>
        rmot_run(chains = 1, iter = 1000, verbose = FALSE, show_messages = FALSE)
    )
  }

  # Extract samples and check if parameter estimates are reasonable.
  ind_samples <- rstan::extract(single_ind_test, permuted = TRUE,
                                         inc_warmup = FALSE)
  par_ests <- c()
  for(i in seq_along(par_names)){
    par_ests[i] <- mean(ind_samples[[par_names[i]]])
  }

  initial_condition <- mean(ind_samples$ind_y_0)
  expect_equal(par_ests,
               as.numeric(data$single_true_data$DE_pars),
               tolerance = 1e-1)
  expect_equal(initial_condition,
               as.numeric(data$single_true_data$initial_conditions),
               tolerance = 1e-1)

  # hecks for output existence and type
  expect_visible(single_ind_test)
  expect_s4_class(single_ind_test, "stanfit")
}

rmot_test_multi_individual <- function(model_name, data, est_dim){
  if(! is.null(data$step_size)){
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
                         y_0_obs = data$y_0_obs #vector length N_ind
        ) |>
        rmot_run(chains = 2, iter = 100, verbose = FALSE, show_messages = FALSE)
    )
  } else {
    # Test multi-individual without step_size
    suppressWarnings( #Suppresses stan warnings
      multi_ind_test <- rmot_model(paste0(model_name, "_multi_ind")) |>
        rmot_assign_data(n_obs = data$n_obs, #integer
                         n_ind = data$n_ind, #integer
                         y_obs = data$y_obs, #vector length N_obs
                         obs_index = data$obs_index, #vector length N_obs
                         time = data$time, #Vector length N_obs
                         ind_id = data$ind_id, #Vector length N_obs
                         y_0_obs = data$y_0_obs #vector length N_ind
        ) |>
        rmot_run(chains = 2, iter = 100, verbose = FALSE, show_messages = FALSE)
    )
  }

  # Extract samples
  multi_samples <- rstan::extract(multi_ind_test, permuted = FALSE, inc_warmup = TRUE)
  expect_equal(dim(multi_samples), c(100, 2, est_dim))

  expect_visible(multi_ind_test)
  expect_s4_class(multi_ind_test, "stanfit")
}
