#Testing for linear model
test_that("Model structures: linear", {
  # Single individual
  single_model <- hmde_model("linear_single_ind")
  expect_named(single_model, c("step_size", "n_obs", "y_obs", "obs_index",
                               "time", "y_bar", "int_method", "model"))
  expect_type(single_model, "list")
  expect_visible(single_model)
})
