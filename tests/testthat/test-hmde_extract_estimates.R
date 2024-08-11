test_that("Execution and output: Constant single ind", {
  data <- data.frame(
    y_obs = c(1.1, 2.0, 2.9),
    obs_index = c(1,2, 3),
    time = c(0,1, 2),
    ind_id = c(1, 1, 1)
  )

  constant_single_fit <- hmde_model("constant_single_ind") |>
    hmde_assign_data(data = data) |>
    hmde_run(chains = 1, iter = 20, cores = 1,
             verbose = FALSE, show_messages = FALSE)

  output <- hmde_extract_estimates(model = "constant_single_ind",
                                   fit = constant_single_fit,
                                   input_measurement_data = data)

  expect_named(constant_single)

  expect_visible(constant_single)

  expect_type(constant_single, "list")

  constant_empty <- hmde_model("constant_single_ind")

  expect_null(constant_empty$y_obs)

  expect_true(length(constant_single$y_obs) > 0)
})
