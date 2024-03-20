# Create testing data for other models
## Single
true_single <- seq(from=1, by=2.5, length.out=7)
set.seed(2024)
y_single <- true_single + true_single*rnorm(n=7, 0, 0.02) + rnorm(n=7, 0, 0.2)

## Multi
true_multi <- c(seq(from=1, by=2.5, length.out=7),
                seq(from=2, by=2, length.out=7))
set.seed(2024)
y_multi <- true_multi + true_multi*rnorm(n=14, 0, 0.02) + rnorm(n=14, 0, 0.2)

# Create table of summary estimates for linear model
set.seed(2024)
suppressWarnings( #Suppresses stan warnings
  lm_test <- rmot_model("linear") |>
    rmot_assign_data(X = Loblolly$age,
                     Y = Loblolly$height,
                     N = nrow(Loblolly)) |>
    rmot_run(chains = 2, iter = 300, verbose = FALSE, show_messages = FALSE)
)

samps_linear_summary <- rstan::summary(lm_test)$summary

# Create table of summary estimates for constant single indiv model
set.seed(2024)
suppressWarnings( #Suppresses stan warnings
  constant_single_ind_test <- rmot_model("constant_single_ind") |>
    rmot_assign_data(n_obs = 7, #integer
                     y_obs = seq(from=1, by=2.5, length.out=7) + rnorm(n=7, 0, 0.2),
                     obs_index = 1:7, #vector length N_obs
                     time = rep(5, times=length(y_single)), #Vector length N_obs
                     y_0_obs = y_single[1] #vector length N_ind
    ) |>
    rmot_run(chains = 2, iter = 300, verbose = FALSE, show_messages = FALSE)
)

samps_const_single_ind_summary <- rstan::summary(constant_single_ind_test)$summary


# Create table of summary estimates for  constant multi-individual
set.seed(2024)

suppressWarnings( #Suppresses stan warnings
  constant_multi_ind_test <- rmot_model("constant_multi_ind") |>
    rmot_assign_data(n_obs = length(y_multi), #integer
                     n_ind = 2, #integer
                     y_obs = y_multi, #vector length N_obs
                     obs_index = rep(seq(from=1, to=7, by=1), times=2), #vector length N_obs
                     time = rep(5, times=length(y_multi)), #Vector length N_obs
                     ind_id = c(rep(1, times=7), rep(2, times=7)), #Vector length N_obs
                     y_0_obs = y_multi[c(1, 8)] #vector length N_ind
    ) |>
    rmot_run(chains = 2, iter = 300, verbose = FALSE, show_messages = FALSE)
)

samps_const_multi_ind_summary <- rstan::summary(constant_multi_ind_test)$summary


# Save the data
usethis::use_data(y_single, y_multi,
                  samps_linear_summary, samps_const_single_ind_summary, samps_const_multi_ind_summary,
                  overwrite = TRUE, internal = TRUE)
