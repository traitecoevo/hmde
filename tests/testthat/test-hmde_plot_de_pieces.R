test_that("Execution and output: plot_de_pieces function", {
  suppressWarnings(
    fit <- hmde_data_template("constant_multi_ind",
                              obs_data = Trout_Size_Data[1:8,]) |>
      hmde_run(chains = 1, iter = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- hmde_estimates(fit,
                           obs_data = Trout_Size_Data[1:8,])

  plot <- hmde_plot_de_pieces(output,
                              xlab = "S(t)",
                              ylab = "g",
                              title = "Constant growth")

  expect_visible(plot)

  expect_true(is_ggplot(plot))
})


test_that("Execution and output: bad input", {
  suppressWarnings(
    fit <- hmde_data_template("constant_multi_ind",
                              obs_data = Trout_Size_Data[1:8,]) |>
      hmde_run(chains = 1, iter = 1,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- hmde_estimates(fit,
                           obs_data = Trout_Size_Data[1:8,])
  output_error <- output
  model_name(output_error) <- "not_a_model"
  expect_error(
    hmde_plot_de_pieces(output_error)
  )

  model_name(output_error) <- NA_character_
  expect_error(
    hmde_plot_de_pieces(output_error)
  )

  output_error <- output
  individual_ests(output_error) <- tibble(NA)
  expect_error(
    hmde_plot_de_pieces(output_error)
  )

  output_error <- output
  measurement_ests(output_error) <- tibble(NA)
  expect_error(
    hmde_plot_de_pieces(output_error)
  )
})
