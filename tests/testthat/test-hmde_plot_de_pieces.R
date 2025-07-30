test_that("Execution and output: plot_de_pieces function", {
  suppressWarnings(
    multi_ind_trout <- hmde_model("constant_multi_ind") |>
      hmde_assign_data(data = Trout_Size_Data) |>
      hmde_run(chains = 1, iter = 50, cores = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- hmde_extract_estimates(fit = multi_ind_trout,
                                   input_measurement_data = Trout_Size_Data)

  plot <- hmde_plot_de_pieces(output,
                      xlab = "S(t)",
                      ylab = "g",
                      title = "Constant growth")

  expect_named(plot)

  expect_visible(plot)

  expect_true(is_ggplot(plot))
})


test_that("Execution and output: bad input", {
  suppressWarnings(
    multi_ind_trout <- hmde_model("constant_multi_ind") |>
      hmde_assign_data(data = Trout_Size_Data) |>
      hmde_run(chains = 1, iter = 50, cores = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- hmde_extract_estimates(fit = multi_ind_trout,
                                   input_measurement_data = Trout_Size_Data)

  output_error <- output
  output_error$model_name <- "not_a_model"
  expect_error(
    hmde_plot_de_pieces(output_error)
  )

  output_error$model_name <- NULL
  expect_error(
    hmde_plot_de_pieces(output_error)
  )

  output_error <- output
  output_error$individual_data <- NULL
  expect_error(
    hmde_plot_de_pieces(output_error)
  )

  output_error <- output
  output_error$measurement_data <- NULL
  expect_error(
    hmde_plot_de_pieces(output_error)
  )
})
