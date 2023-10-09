test_that("Execution and output", {
  expect_named(rmot_model("linear"))
  expect_type(rmot_model("linear"), "list")
  expect_visible(rmot_model("linear"))

  expect_named(rmot_model("constant_single"))
  expect_type(rmot_model("constant_single"), "list")
  expect_visible(rmot_model("constant_single"))

  lm_test <- rmot_model("linear") |>
    rmot_assign_data(X = Loblolly$age,
                     Y = Loblolly$height,
                     N = nrow(Loblolly)) |>
    rmot_run(chains = 2, iter = 300, verbose = FALSE, show_messages = FALSE)

  expect_visible(lm_test)

  expect_s4_class(lm_test, "stanfit")
})
