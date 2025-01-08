test_that("Execution and output: extract_estimates function", {
  data <- data.frame(
    y_obs = c(1.1, 2.0, 2.9),
    obs_index = c(1, 2, 3),
    time = c(0, 1, 2),
    ind_id = c(1, 1, 1)
  )

  suppressWarnings(
    output <- hmde_model("constant_single_ind") |>
      hmde_assign_data(data = data) |>
      hmde_run(chains = 1, iter = 20, cores = 1,
               verbose = FALSE, show_messages = FALSE) |>
      hmde_extract_estimates(model = "constant_single_ind",
                             input_measurement_data = data)
  )

  expect_named(output)

  expect_visible(output)

  expect_type(output, "list")
})
