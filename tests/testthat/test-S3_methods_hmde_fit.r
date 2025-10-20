#Testing for plot function
test_that("hmde_model_template: plot function for single ind models", {
  suppressWarnings(
    fit <- hmde_model("constant_single_ind") |>
      hmde_assign_data(Trout_Size_Data[1:5,])|>
      hmde_run(chains = 1, iter = 100,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- plot.hmde_fit(fit)

  expect_visible(output)

  expect_true(is_ggplot(output))
})

test_that("hmde_model_template: plot function for multiple-ind models", {
  suppressWarnings(
    fit <- hmde_model("constant_multi_ind") |>
      hmde_assign_data(Trout_Size_Data)|>
      hmde_run(chains = 1, iter = 100,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- plot.hmde_fit(fit)

  expect_visible(output)

  expect_true(is_ggplot(output))
})
