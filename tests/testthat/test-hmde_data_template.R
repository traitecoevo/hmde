test_that("hmde_data_template: initialisaiton and validation", {
  # Default initialisation should not error
  expect_no_error(new("hmde_data_template"))

  #Check class
  test_object <- new("hmde_data_template")
  expect_true(is(test_object, "hmde_data_template"))

  # Attempt to assign to non-existent field
  expect_error(new("hmde_data_template", invalid_field = 1))

  # Test internal type validation
  expect_error(new("hmde_data_template", model_name = 1))
  expect_error(new("hmde_data_template", model_level = 1))
  expect_error(new("hmde_data_template", obs_data = 1))
  expect_error(new("hmde_data_template", prior_pars = 1))

  # Test length validation for model_name and model_type
  expect_error(new("hmde_data_template", model_name = c("one", "two")))
  expect_error(new("hmde_data_template", model_type = c("one", "two")))
})

test_that("hmde_data_template: setters and getters", {
  test_object <- new("hmde_data_template")

  # model_name
  model_name(test_object) <- "test_value"
  expect_equal(model_name(test_object), "test_value")
  expect_error(model_name(test_object) <- 1) # Bad type

  # model_level
  model_level(test_object) <- "test_value"
  expect_equal(model_level(test_object), "test_value")
  expect_error(model_level(test_object) <- 1) # Bad type

  # obs_data
  obs_data(test_object) <- list(test_name = "test_value")
  expect_equal(obs_data(test_object), list(test_name = "test_value"))
  expect_error(obs_data(test_object) <- 1) # Bad type

  # prior_pars
  prior_pars(test_object) <- list(test_name = "test_value")
  expect_equal(prior_pars(test_object), list(test_name = "test_value"))
  expect_error(obs_data(test_object) <- 1) # Bad type
})

test_that("hmde_data_template: constructor", {
  # Just model name provided
  test_object <- hmde_data_template(model_name = "constant_single_ind")
  # Check default values
  expect_equal(model_name(test_object), "constant_single_ind")
  expect_equal(model_level(test_object), "single_ind")
  expect_equal(obs_data(test_object), list(n_obs = NA_integer_,
                                           y_obs = NA_real_,
                                           obs_index = NA_integer_,
                                           time = NA_real_))
  expect_equal(prior_pars(test_object), list(
    prior_pars_ind_beta = c(0, 2),
    prior_pars_global_error_sigma = c(0, 2)
  ))

  # Invalid model name
  expect_error(hmde_data_template(model_name = "wrong_name"))

  # Testing different methods of data assignment
  # Filling obs_data from list
  obs_data <- list(
    n_obs = 3,
    y_obs = c(1,1,1),
    obs_index = 1:3,
    time = 0:2
  )
  test_object <- hmde_data_template(model_name = "constant_single_ind",
                                    obs_data = obs_data)
  expect_equal(obs_data(test_object), obs_data)

  # Filling obs_data from tibble
  test_object <- hmde_data_template(model_name = "constant_single_ind",
                                    obs_data = Trout_Size_Data[1:4,])
  expect_equal(obs_data(test_object),
               list(
                 n_obs = 4,
                 y_obs = Trout_Size_Data[["y_obs"]][1:4],
                 obs_index = Trout_Size_Data[["obs_index"]][1:4],
                 time = Trout_Size_Data[["time"]][1:4])
               )

  # Fill obs_data elements individually
  test_object <- hmde_data_template(model_name = "constant_single_ind",
                                    n_obs = 3,
                                    y_obs = c(1,1,1),
                                    obs_index = 1:3,
                                    time = 0:2)
  expect_equal(obs_data(test_object), obs_data)

  test_object <- hmde_data_template(model_name = "constant_single_ind",
                                    y_obs = c(1,1,1),
                                    obs_index = 1:3,
                                    time = 0:2)
  expect_equal(obs_data(test_object), obs_data)

  #Mismatched vector lengths
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  y_obs = c(1),
                                  obs_index = 1:3,
                                  time = 0:2))
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  y_obs = c(1,1,1),
                                  obs_index = 1,
                                  time = 0:2))
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  y_obs = c(1,1,1),
                                  obs_index = 1:3,
                                  time = 0))

  #Missing data
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  obs_index = 1:3,
                                  time = 0:2))
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  y_obs = c(1,1,1),
                                  time = 0:2))
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  y_obs = c(1,1,1),
                                  obs_index = 1:3))

  #Attempt to make single-ind model with multiple individuals
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  obs_data = Trout_Size_Data))

  #Filling prior_pars
  prior_pars <- list(
    prior_pars_ind_beta = c(1,2),
    prior_pars_global_error_sigma = c(1,3)
  )
  test_object <- hmde_data_template(model_name = "constant_single_ind",
                                    prior_pars = prior_pars)
  expect_equal(prior_pars(test_object), prior_pars)

  test_object <- hmde_data_template(model_name = "constant_single_ind",
                                    prior_pars_ind_beta = c(1,2),
                                    prior_pars_global_error_sigma = c(1,3))
  expect_equal(prior_pars(test_object), prior_pars)

  #Modify only one
  test_object <- hmde_data_template(model_name = "constant_single_ind",
                                    prior_pars_ind_beta = c(1,2))
  expect_equal(prior_pars(test_object), list(
    prior_pars_ind_beta = c(1,2),
    prior_pars_global_error_sigma = c(0,2)
  ))

  #Bad prior inputs
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  prior_pars = list(bad_name = 1)))
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  prior_pars =
                                    list(prior_pars_ind_beta = c(1,2),
                                         prior_pars_global_error_sigma = c(1,3),
                                         extra_entry = 1)))
  expect_error(hmde_data_template(model_name = "constant_single_ind",
                                  prior_pars =
                                    list(prior_pars_ind_beta = c(1,2),
                                         prior_pars_global_error_sigma = 1)))

  # Fill both obs_data and priors
  obs_data <- list(
    n_obs = 3,
    y_obs = c(1,1,1),
    obs_index = 1:3,
    time = 0:2
  )
  prior_pars <- list(
    prior_pars_ind_beta = c(1,2),
    prior_pars_global_error_sigma = c(1,3)
  )

  test_object <- hmde_data_template(model_name = "constant_single_ind",
                                    obs_data = obs_data,
                                    prior_pars = prior_pars)
  expect_equal(prior_pars(test_object), prior_pars)
  expect_equal(obs_data(test_object), obs_data)
})


test_that("hmde_data_template: show", {
 test_object <- hmde_data_template("constant_single_ind")
 expect_no_error(show(test_object))
})

test_that("hmde_data_template: print", {
  test_object <- hmde_data_template("constant_single_ind")
  expect_no_error(print(test_object))
})

test_that("hmde_data_template: summary", {
  test_object <- hmde_data_template("constant_single_ind")
  expect_no_error(summary(test_object))

  #Test with observation data
  obs_data <-list(
    n_obs = 30,
    y_obs = 1:30,
    obs_index = 1:30,
    time = 0:29
  )
  test_object <- hmde_data_template("constant_single_ind",
                                    obs_data = obs_data)
  expect_no_error(summary(test_object))

  #Test multi-species model with observation data
  obs_data <-list(
    n_obs = 30,
    y_obs = 1:30,
    obs_index = 1:30,
    time = 0:29,
    ind_id = rep(1, times = 30)
  )
  test_object <- hmde_data_template("constant_multi_ind",
                                    obs_data = obs_data)
  expect_no_error(summary(test_object))
})

test_that("hmde_data_template: plot", {
  test_object <- hmde_data_template("constant_single_ind")
  expect_no_error(plot(test_object))
})
