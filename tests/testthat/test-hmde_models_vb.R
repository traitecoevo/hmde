#Testing for vb model
test_that("Model structures: von Bertalanffy", {
  # Single individual
  single_model <- hmde_model("vb_single_ind")
  expect_true(inherits(single_model, "hmde_data_template"))
  expect_visible(single_model)

  #Multiple individuals
  multi_model <- hmde_model("vb_multi_ind")
  expect_true(inherits(single_model, "hmde_data_template"))
  expect_visible(multi_model)
})

test_that("Data assignment: VB", {
  # Single individual
  test_data <- hmde_data_template("vb_single_ind",
                                  obs_data = Trout_Size_Data[1:4,])
  expect_true(inherits(test_data, "hmde_data_template"))
  expect_visible(test_data)

  #Multiple individuals
  test_data <- hmde_data_template("vb_multi_ind",
                                  obs_data = Trout_Size_Data[1:8,])
  expect_true(inherits(test_data, "hmde_data_template"))
  expect_visible(test_data)
})

test_that("Execution: von Bertalanffy fit", {
  #Model running single ind
  expect_no_error(
    suppressWarnings(
      fit <- hmde_data_template("vb_single_ind",
                                obs_data = Trout_Size_Data[1:4,]) |>
        hmde_run(chains = 1, iter = 1,
                 verbose = FALSE, show_messages = FALSE)
    )
  )

  #Model running multi-ind
  expect_no_error(
    suppressWarnings(
      fit <- hmde_data_template("vb_multi_ind",
                                obs_data = Trout_Size_Data[1:12,]) |>
        hmde_run(chains = 1, iter = 1,
                 verbose = FALSE, show_messages = FALSE)
    )
  )
})
