test_that("Execution and output: plot_Rhat_hist function", {
  suppressWarnings(
    fit <- hmde_model("constant_single_ind") |>
      hmde_assign_data(data = Trout_Size_Data[1:4,]) |>
      hmde_run(chains = 1, iter = 50, cores = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  plot <- hmde_plot_Rhat_hist(fit)

  expect_visible(plot)

  expect_true(is_ggplot(plot))
})


test_that("Execution and output: bad input", {
  suppressWarnings(
    fit <- hmde_model("constant_single_ind") |>
      hmde_assign_data(data = Trout_Size_Data[1:4,]) |>
      hmde_run(chains = 1, iter = 50, cores = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  output_error <- fit
  output_error@model_name <- "not_a_model"
  expect_error(
    hmde_plot_Rhat_hist(output_error)
  )
})
