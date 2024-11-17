#Testing for power model
test_that("Model structures: power", {
  # Single individual
  single_model <- hmde_model("power_single_ind")
  expect_named(single_model, c("step_size", "n_obs", "y_obs", "obs_index",
                               "time", "y_0_obs", "y_bar", "model"))
  expect_type(single_model, "list")
  expect_visible(single_model)

  #Multiple individuals
  multi_model <- hmde_model("power_multi_ind")
  expect_named(multi_model, c("step_size", "n_obs", "n_ind", "y_obs",
                              "obs_index", "time", "ind_id",
                              "y_0_obs", "y_bar", "model"))
  expect_type(multi_model, "list")
  expect_visible(multi_model)
})
