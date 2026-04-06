#Testing for affine model
test_that("Model structures: affine", {
  # Single individual
  single_model <- hmde_model("affine_single_ind")
  expect_true(inherits(single_model, "hmde_data_template"))
  expect_visible(single_model)
})
