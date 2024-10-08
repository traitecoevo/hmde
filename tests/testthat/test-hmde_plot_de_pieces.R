test_that("Execution and output: plot_de_pieces function", {
  suppressWarnings(
    multi_ind_trout <- hmde_model("constant_multi_ind") |>
      hmde_assign_data(data = Trout_Size_Data) |>
      hmde_run(chains = 1, iter = 50, cores = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- hmde_extract_estimates(model = "constant_multi_ind",
                                   fit = multi_ind_trout,
                                   input_measurement_data = Trout_Size_Data)

  plot <- hmde_plot_de_pieces(model = "constant_multi_ind",
                      individual_data = output$individual_data,
                      measurement_data = output$measurement_data,
                      xlab = "S(t)",
                      ylab = "g",
                      title = "Constant growth")

  expect_named(plot)

  expect_visible(plot)

  expect_type(plot, "list")
})


test_that("Execution and output: bad input", {
  suppressWarnings(
    multi_ind_trout <- hmde_model("constant_multi_ind") |>
      hmde_assign_data(data = Trout_Size_Data) |>
      hmde_run(chains = 1, iter = 50, cores = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- hmde_extract_estimates(model = "constant_multi_ind",
                                   fit = multi_ind_trout,
                                   input_measurement_data = Trout_Size_Data)

  expect_error(
    hmde_plot_de_pieces(model = "not_a_model",
                              individual_data = output$individual_data,
                              measurement_data = output$measurement_data)
  )

  expect_error(
    hmde_plot_de_pieces(model = NULL,
                        individual_data = output$individual_data,
                        measurement_data = output$measurement_data)
  )

  expect_error(
    hmde_plot_de_pieces(model = "constant_multi_ind",
                        individual_data = NULL,
                        measurement_data = output$measurement_data)
  )

  expect_error(
    hmde_plot_de_pieces(model = "constant_multi_ind",
                        individual_data = output$individual_data,
                        measurement_data = NULL)
  )
})
