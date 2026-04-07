#Testing for affine model
test_that("Model structures: affine", {
  # Single individual
  single_model <- hmde_model("affine_single_ind")
  expect_true(inherits(single_model, "hmde_data_template"))
  expect_visible(single_model)
})

test_that("Data assignment: affine", {
  # Single individual
  test_data <- hmde_data_template("affine_single_ind",
                                  obs_data = Trout_Size_Data[1:4,],
                                  step_size = 1,
                                  int_method = 1)
  expect_true(inherits(test_data, "hmde_data_template"))
  expect_visible(test_data)
})

test_that("Execution: affine fit", {
  #Model running single ind
  expect_no_error(
    suppressWarnings(
      fit <- hmde_data_template("affine_single_ind",
                                obs_data = Trout_Size_Data[1:4,],
                                step_size = 1,
                                int_method = 1) |>
        hmde_run(chains = 1, iter = 1,
                 verbose = FALSE, show_messages = FALSE)
    )
  )
})
