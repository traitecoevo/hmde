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

test_that("Execution and output: Linear", {

  # Test linear model
  set.seed(2024)
  suppressWarnings( #Suppresses stan warnings
    lm_test <- rmot_model("linear") |>
      rmot_assign_data(X = Loblolly$age,
                       Y = Loblolly$height,
                       N = nrow(Loblolly)) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  expect_visible(lm_test)
  expect_s4_class(lm_test, "stanfit")
})

test_that("Execution: Constant single individual", {



  # Test constant single individual
  set.seed(2024)

  suppressWarnings( #Suppresses stan warnings
    constant_single_ind_test <- rmot_model("constant_single_ind") |>
      rmot_assign_data(n_obs = 7, #integer
                       y_obs = seq(from=1, by=2.5, length.out=7) + rnorm(n=7, 0, 0.2),
                       obs_index = 1:7, #vector length N_obs
                       time = rep(5, times=length(y_single)), #Vector length N_obs
                       y_0_obs = y_single[1] #vector length N_ind
      ) |>
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
  )

  expect_visible(constant_single_ind_test)
  expect_s4_class(constant_single_ind_test, "stanfit")

})

test_that("Execution: Constant multiple individuals", {

    # Test constant multi-individual
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
      rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
)

  expect_visible(constant_multi_ind_test)
  expect_s4_class(constant_multi_ind_test, "stanfit")
})
