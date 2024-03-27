# Make some data
lm_data <- data.frame(X = Loblolly$age,
                      Y = Loblolly$height,
                      N = nrow(Loblolly))

saveRDS(lm_data, file = "tests/testthat/fixtures/linear/lm_data.rds")

# Run a baseline model for the data
suppressWarnings( #Suppresses stan warnings
  lm_baseline <- rmot_model("linear") |>
    rmot_assign_data(X = lm_data$X,
                     Y = lm_data$Y,
                     N = nrow(lm_data)) |>
    rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE, seed=1)
)

# Save output to compare as
lm_baseline_output <- rstan::summary(lm_baseline)$summary

saveRDS(lm_baseline_output, "tests/testthat/fixtures/linear/lm_baseline_output.rds")
