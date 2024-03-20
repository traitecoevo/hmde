test_that("Model structures", {
  expect_named(rmot_model("linear"))
  expect_type(rmot_model("linear"), "list")
  expect_visible(rmot_model("linear"))

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

test_that("Execution and output", {
  # Test linear model
  set.seed(2024)
  suppressWarnings( #Suppresses stan warnings
    lm_test <- rmot_model("linear") |>
      rmot_assign_data(X = Loblolly$age,
                       Y = Loblolly$height,
                       N = nrow(Loblolly)) |>
      rmot_run(chains = 2, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  #Samples should be the same every time as seed is set
  samps <- rstan::extract(lm_test, permuted=FALSE, inc_warmup=TRUE)
  expect_snapshot(samps)
  expect_visible(lm_test)
  expect_s4_class(lm_test, "stanfit")

  # Test constant single individual
  set.seed(2024)
  suppressWarnings( #Suppresses stan warnings
    lm_test <- rmot_model("linear") |>
      rmot_assign_data(X = Loblolly$age,
                       Y = Loblolly$height,
                       N = nrow(Loblolly)) |>
      rmot_run(chains = 2, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  #Samples should be the same every time as seed is set
  samps <- rstan::extract(lm_test, permuted=FALSE, inc_warmup=TRUE)
  expect_snapshot(samps)
  expect_visible(lm_test)
  expect_s4_class(lm_test, "stanfit")

  # Test constant multi-individual
  set.seed(2024)
  suppressWarnings( #Suppresses stan warnings
    lm_test <- rmot_model("linear") |>
      rmot_assign_data(X = Loblolly$age,
                       Y = Loblolly$height,
                       N = nrow(Loblolly)) |>
      rmot_run(chains = 2, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  #Samples should be the same every time as seed is set
  samps <- rstan::extract(lm_test, permuted=FALSE, inc_warmup=TRUE)
  expect_snapshot(samps)
  expect_visible(lm_test)
  expect_s4_class(lm_test, "stanfit")
})
