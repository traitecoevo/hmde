#Testing for print function
test_that("hmde_model_template: print function", {
  output <- hmde_model("constant_single_ind") |>
    print.hmde_model_template()
})

#Testing for summary function
test_that("hmde_model_template: print function", {
  output <- hmde_model("constant_single_ind") |>
    summary.hmde_model_template()
})


#Testing for plot function
test_that("hmde_model_template: print function", {
  output <- hmde_model("constant_single_ind") |>
    plot.hmde_model_template()
})
