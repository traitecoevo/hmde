test_that("Execution and output", {
  constant_single <- hmde_model("constant_single_ind") |>
    hmde_assign_data(n_obs = 2,
                     y_obs = c(1,2),
                     obs_index = c(1,2),
                     time = c(0,1),
                     y_0_obs = 1)
  
  expect_named(constant_single)
  
  expect_visible(constant_single)
  
  expect_type(constant_single, "list")
  
  constant_empty <- hmde_model("constant_single_ind")
  
  expect_null(constant_empty$y_obs)
  
  expect_true(length(constant_single$y_obs) > 0)
})
