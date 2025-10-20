#Testing for print function
test_that("hmde_fit: print function for single ind models", {

  suppressWarnings(
    fit <- hmde_model("constant_single_ind") |>
      hmde_assign_data(Trout_Size_Data[1:4,])|>
      hmde_run(chains = 1, iter = 1000,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- print.hmde_fit(fit)
})

test_that("hmde_fit: print function for multi-ind models", {

  suppressWarnings(
    fit <- hmde_model("constant_multi_ind") |>
      hmde_assign_data(Trout_Size_Data)|>
      hmde_run(chains = 1, iter = 1000,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- print.hmde_fit(fit)
})

#Testing for summary function
test_that("hmde_model_template: summary function for single ind models", {
  suppressWarnings(
    fit <- hmde_model("constant_single_ind") |>
      hmde_assign_data(Trout_Size_Data[1:5,])|>
      hmde_run(chains = 1, iter = 1000,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- summary.hmde_fit(fit)
})

test_that("hmde_model_template: summary function for multi-ind models", {
  suppressWarnings(
    fit <- hmde_model("constant_multi_ind") |>
      hmde_assign_data(Trout_Size_Data)|>
      hmde_run(chains = 1, iter = 1000,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- summary.hmde_fit(fit)
})


#Testing for plot function
test_that("hmde_model_template: plot function for single ind models", {
  suppressWarnings(
    fit <- hmde_model("constant_single_ind") |>
      hmde_assign_data(Trout_Size_Data[1:5,])|>
      hmde_run(chains = 1, iter = 1000,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- plot.hmde_fit(fit)
})

test_that("hmde_model_template: plot function for multiple-ind models", {
  suppressWarnings(
    fit <- hmde_model("constant_multi_ind") |>
      hmde_assign_data(Trout_Size_Data)|>
      hmde_run(chains = 1, iter = 1000,
               verbose = FALSE, show_messages = FALSE)
  )

  output <- plot.hmde_fit(fit)
})
