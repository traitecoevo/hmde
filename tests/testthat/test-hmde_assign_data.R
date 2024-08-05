test_that("Execution and output: Constant single ind", {
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

test_that("Execution and output: Constant multi ind manual input", {
  #Full manual input
  constant_multi <- hmde_model("constant_multi_ind") |>
    hmde_assign_data(n_obs = nrow(Trout_Size_Data),         #integer
                     n_ind = max(Trout_Size_Data$ind_id),   #integer
                     y_obs = Trout_Size_Data$y_obs,         #vector length N_obs
                     obs_index = Trout_Size_Data$obs_index, #vector length N_obs
                     time = Trout_Size_Data$time,           #Vector length N_obs
                     ind_id = Trout_Size_Data$ind_id,       #Vector length N_obs
                     y_0_obs = Trout_Size_Data$y_obs[which(Trout_Size_Data$obs_index == 1)]        #Vector length N_ind
    )

  expect_named(constant_multi)

  expect_visible(constant_multi)

  expect_type(constant_multi, "list")

  constant_empty <- hmde_model("constant_multi_ind")

  expect_null(constant_empty$y_obs)

  expect_true(length(constant_multi$y_obs) > 0)
})

test_that("Execution and output: Constant multi ind minimum manual input", {
  #Full manual input
  constant_multi <- hmde_model("constant_multi_ind") |>
    hmde_assign_data(y_obs = Trout_Size_Data$y_obs,         #vector length N_obs
                     obs_index = Trout_Size_Data$obs_index, #vector length N_obs
                     time = Trout_Size_Data$time,           #Vector length N_obs
                     ind_id = Trout_Size_Data$ind_id,       #Vector length N_obs
                     )

  expect_named(constant_multi)

  expect_visible(constant_multi)

  expect_type(constant_multi, "list")

  expect_true(length(constant_multi$y_obs) > 0)
})

test_that("Execution and output: Constant multi ind tibble input", {
  #Full manual input
  constant_multi <- hmde_model("constant_multi_ind") |>
    hmde_assign_data(data = Trout_Size_Data)

  expect_named(constant_multi)

  expect_visible(constant_multi)

  expect_type(constant_multi, "list")

  expect_true(length(constant_multi$y_obs) > 0)
})

test_that("Execution and output: bad input", {
  #Full manual input
  expect_error( hmde_model("constant_multi_ind") |>
    hmde_assign_data(data = c(0,1))
  )
})
