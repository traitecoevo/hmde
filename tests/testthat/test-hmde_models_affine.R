#Testing for affine model
test_that("Model structures: affine", {
  # Single individual
  single_model <- hmde_model("affine_single_ind")
  expect_named(single_model, c("step_size", "n_obs", "y_obs", "obs_index",
                               "time", "int_method", "y_bar", "prior_means",
                               "prior_sds", "model"))
  expect_type(single_model, "list")
  expect_visible(single_model)
})
