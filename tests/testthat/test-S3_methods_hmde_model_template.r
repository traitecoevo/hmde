#Testing for print function
test_that("Model structures: affine", {
  output <- hmde_model("constant_single_ind") |> print.hmde_model_template()
})
