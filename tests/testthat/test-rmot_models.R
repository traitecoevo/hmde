test_that("Model structures: Linear", {
  expect_named(rmot_model("linear"))
  expect_type(rmot_model("linear"), "list")
  expect_visible(rmot_model("linear"))
})

test_that("Execution and output: Linear", {
  lm_data <- readRDS(test_path("fixtures", "linear", "lm_data.rds"))
  lm_baseline_output <- readRDS(test_path("fixtures", "linear", "lm_baseline_output.rds"))

  # Test linear model
  suppressWarnings( #Suppresses stan warnings
    lm_test <- rmot_model("linear") |>
      rmot_assign_data(X = lm_data$X,
                       Y = lm_data$Y,
                       N = nrow(lm_data)) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE,
      seed = 1)
  )

  # Extract samples
  lm_samples <- rstan::extract(lm_test, permuted = FALSE, inc_warmup = FALSE) |>
    as.data.frame() |>
    head(n=100)

  expect_equal(lm_samples, lm_baseline_output, tolerance = 1e-5)
  expect_visible(lm_test)
  expect_s4_class(lm_test, "stanfit")
})
