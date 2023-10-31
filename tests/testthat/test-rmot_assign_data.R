test_that("Setup and assign data", {

  # test a bad ipout
  expect_error(rmot_model("clearly wrong"))

  expected_names <- c("X","Y", "N", "model")

  expect_silent(lm_loblloy <- rmot_model("linear"))
  expect_type(lm_loblloy, "list")
  expect_named(lm_loblloy, expected_names)
  for(v in c("X", "Y", "N"))
    expect_null(lm_loblloy[[v]])
  expect_equal(lm_loblloy$model, "linear")

  expect_silent(
    lm_loblloy <- rmot_model("linear") |>
      rmot_assign_data(X = Loblolly$age)
  )
  expect_named(lm_loblloy, c("X","Y", "N", "model"))
  expect_equal(lm_loblloy$X, Loblolly$age)
  expect_null(lm_loblloy$Y)
  expect_null(lm_loblloy$N)
  expect_equal(lm_loblloy$model, "linear")

  expect_true(length(lm_loblloy$X) > 0)



  # Constant models
  # Single individual
  expect_named(rmot_model("constant_single_ind"))
  expect_type(rmot_model("constant_single_ind"), "list")
  expect_visible(rmot_model("constant_single_ind"))

  #Multiple individuals
  expect_named(rmot_model("constant_multi_ind"))
  expect_type(rmot_model("constant_multi_ind"), "list")
  expect_visible(rmot_model("constant_multi_ind"))

})
