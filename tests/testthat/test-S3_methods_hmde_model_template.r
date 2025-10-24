#Testing for plot function
test_that("hmde_model_template: print function", {
  output <- hmde_model("constant_single_ind") |>
    plot.hmde_model_template()

  expect_visible(output)

  expect_true(is_ggplot(output))
})
