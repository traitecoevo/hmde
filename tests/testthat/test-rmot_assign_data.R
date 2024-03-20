test_that("Execution and output", {

  lm_loblloy <- rmot_model("linear") |>
    rmot_assign_data(X = Loblolly$age)

  expect_named(lm_loblloy)

  expect_visible(lm_loblloy)

  expect_type(lm_loblloy, "list")

  lm_empty <- rmot_model("linear")

  expect_null(lm_empty$X)

  expect_true(length(lm_loblloy$X) > 0)
})
