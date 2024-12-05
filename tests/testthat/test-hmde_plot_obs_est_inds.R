test_that("Execution and output: plot_obs_est_inds function", {
  plot <- hmde_plot_obs_est_inds(n_ind_to_plot = 2,
                                 measurement_data = Tree_Size_Ests$measurement_data)

  expect_named(plot)

  expect_visible(plot)

  expect_type(plot, "list")
})


test_that("Execution and output: bad input", {
  expect_error(
    hmde_plot_obs_est_inds(measurement_data = Tree_Size_Ests$measurement_data)
  )

  expect_error(
    hmde_plot_obs_est_inds()
  )

  expect_error(
    hmde_plot_obs_est_inds(individual_data = Tree_Size_Ests$measurement_data)
  )

  expect_error(
    hmde_plot_obs_est_inds(n_ind_to_plot = 10^3,
      measurement_data = Tree_Size_Ests$measurement_data)
  )
})
