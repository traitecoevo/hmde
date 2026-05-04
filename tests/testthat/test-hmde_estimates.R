test_that("Execution and output: hmde_estimates", {
  data <- tibble(
    y_obs = c(1.1, 2.0, 2.9),
    obs_index = c(1, 2, 3),
    time = c(0, 1, 2),
    ind_id = c(1, 1, 1)
  )

  suppressWarnings(
    fit <- hmde_data_template("constant_single_ind",
                              obs_data = data) |>
      hmde_run(chains = 1, iter = 20, cores = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  #Typical use
  expect_no_error(hmde_estimates(fit, obs_data = data))

  #Don't need obs_index
  expect_no_error(hmde_estimates(fit, obs_data = data[,c(1,3,4)]))

  #Check output works
  output <- hmde_estimates(fit, obs_data = data)

  expect_visible(output)

  expect_true(inherits(output, "hmde_estimates"))
})

test_that("hmde_estimates: setters and getters", {
  #Getters
  testing_data <- Tree_Size_Ests
  expect_no_error(model_name(testing_data))
  expect_no_error(model_level(testing_data))
  expect_no_error(method(testing_data))
  expect_no_error(runtime(testing_data))
  expect_no_error(fit_summary(testing_data))
  expect_no_error(measurement_ests(testing_data))
  expect_no_error(individual_ests(testing_data))
  expect_no_error(population_ests(testing_data))
  expect_no_error(error_ests(testing_data))
  expect_no_error(prior_pars(testing_data))
  expect_no_error(par_names(testing_data))

  #Setters
  expect_no_error(model_name(testing_data) <- "canham_multi_ind")
  expect_no_error(model_level(testing_data) <- "multi_ind")
  expect_no_error(method(testing_data) <- "sampling")
  expect_no_error(runtime(testing_data) <- Tree_Size_Ests@runtime)
  expect_no_error(fit_summary(testing_data) <- Tree_Size_Ests@fit_summary)
  expect_no_error(measurement_ests(testing_data) <- Tree_Size_Ests@measurement_ests)
  expect_no_error(individual_ests(testing_data) <- Tree_Size_Ests@individual_ests)
  expect_no_error(population_ests(testing_data) <- Tree_Size_Ests@population_ests)
  expect_no_error(error_ests(testing_data) <- Tree_Size_Ests@error_ests)
  expect_no_error(prior_pars(testing_data) <- Tree_Size_Ests@prior_pars)
  expect_no_error(par_names(testing_data) <- Tree_Size_Ests@par_names)
})

test_that("Failure conditions: hmde_estimates constructor function", {
  data <- tibble(
    y_obs = c(1.1, 2.0, 2.9),
    obs_index = c(1, 2, 3),
    time = c(0, 1, 2),
    ind_id = c(1, 1, 1)
  )

  suppressWarnings(
    fit <- hmde_data_template("constant_single_ind",
                              obs_data = data) |>
      hmde_run(chains = 1, iter = 20, cores = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  expect_error(hmde_estimates("not_s4_fit",
                              obs_data = data))

  expect_error(hmde_estimates(fit,
                              obs_data = "not_input_data"))

  #Not a built model
  bad_fit <- fit
  bad_fit@model_name <- "not_a_model"
  expect_error(hmde_estimates(bad_fit,
                              obs_data = data))

  #Missing obs data columns
  expect_error(hmde_estimates(bad_fit,
                              obs_data = data[2:4]))
  expect_error(hmde_estimates(bad_fit,
                              obs_data = data[1:2]))
  expect_error(hmde_estimates(bad_fit,
                              obs_data = data[1:3]))

})
