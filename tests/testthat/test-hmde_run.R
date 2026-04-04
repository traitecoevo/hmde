#Testing for hmde_run
test_that("Execution and output: hmde_run", {
  suppressWarnings(
    fit <- hmde_data_template("constant_single_ind",
                     obs_data = Trout_Size_Data[1:4,]) |>
    hmde_run(chains = 1, iter = 1000,
             verbose = FALSE, show_messages = FALSE)
  )
  expect_type(fit, "S4")

  expect_named(fit)

  expect_visible(fit)
})

test_that("Each model: hmde_run", {
  #Constant multi ind
  data <- hmde_data_template("constant_multi_ind",
                             obs_data = Trout_Size_Data)
  expect_no_error(
    suppressWarnings(
      hmde_run(data, chains = 1, iter = 1,
               verbose = FALSE,
               show_messages = FALSE)
     )
  )

  #Canham multi ind
  data <- hmde_data_template("canham_multi_ind",
                             obs_data = Trout_Size_Data)
  expect_no_error(
    suppressWarnings(
      hmde_run(data, chains = 1, iter = 1,
               verbose = FALSE,
               show_messages = FALSE)
    )
  )

  #VB multi ind
  data <- hmde_data_template("vb_multi_ind",
                             obs_data = Trout_Size_Data)
  expect_no_error(
    suppressWarnings(
      hmde_run(data, chains = 1, iter = 1,
               verbose = FALSE,
               show_messages = FALSE)
    )
  )

  #Single ind models
  #Constant single ind
  data <- hmde_data_template("constant_single_ind",
                     obs_data = Trout_Size_Data[1:4,])
  expect_no_error(
    suppressWarnings(
      hmde_run(data, chains = 1, iter = 1,
               verbose = FALSE,
               show_messages = FALSE)
    )
  )

  #Canham single ind
  data <- hmde_data_template("canham_single_ind",
                             obs_data = Trout_Size_Data[1:4,])
  expect_no_error(
    suppressWarnings(
      hmde_run(data, chains = 1, iter = 1,
               verbose = FALSE,
               show_messages = FALSE)
    )
  )

  #VB single ind
  data <- hmde_data_template("vb_single_ind",
                             obs_data = Trout_Size_Data[1:4,])
  expect_no_error(
    suppressWarnings(
      hmde_run(data, chains = 1, iter = 1,
               verbose = FALSE,
               show_messages = FALSE)
    )
  )
})

test_that("Error handling: hmde_run", {
  #Incorrect model name
  data <- hmde_data_template("constant_single_ind",
                     obs_data = Trout_Size_Data[1:4,])
  model_name(data) <- "not_a_model"
  expect_error(hmde_run(data))

  #Missing data
  data <- hmde_data_template("constant_single_ind",
                             obs_data = Trout_Size_Data[1:4,])
  obs_data(data) <- obs_data(data)[-2]
  expect_error(hmde_run(data))

  #Missing prior
  data <- hmde_data_template("constant_single_ind",
                             obs_data = Trout_Size_Data[1:4,])
  prior_pars(data) <- prior_pars(data)[-1]
  expect_error(hmde_run(data))
})
