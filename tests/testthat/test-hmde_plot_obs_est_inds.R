test_that("Execution and output: plot_obs_est_inds function", {
  plot <- hmde_plot_obs_est_inds(Tree_Size_Ests, n_ind_to_plot = 2)
  expect_visible(plot)

  expect_true(is_ggplot(plot))
})


test_that("Execution and output: bad input", {
  expect_error(
    hmde_plot_obs_est_inds(Tree_Size_Ests)
  )

  expect_error(
    hmde_plot_obs_est_inds()
  )

  expect_error(
    hmde_plot_obs_est_inds(individual_data = measurement_ests(Tree_Size_Ests))
  )

  expect_error(
    hmde_plot_obs_est_inds(Tree_Size_Ests, n_ind_to_plot = 10^3)
  )
})
