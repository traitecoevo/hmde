test_that("Execution and output", {

  # Test single individual model: runs and returns consistent results

  set.seed(6)
  iter <- 1000
  lm_test <- rmot_model("linear") |>
    rmot_assign_data(X = Loblolly$age,
                     Y = Loblolly$height,
                     N = nrow(Loblolly)) |>
    rmot_run(chains = 1, iter = iter, verbose = FALSE, show_messages = FALSE)

  #test structure of output is as intended
  expect_visible(lm_test)
  expect_s4_class(lm_test, "stanfit")
  # ..... and more

  pars <- c("intercept", "beta", "sigma")
  expect_named(lm_test, c(pars, "lp__"))

  # test actual values
  theta <- lm_test |> rstan::extract(pars)
  expect_named(theta, pars)
  expect_type(theta, "list")
  beta <- theta$beta
  expect_length(beta, iter/2)
  expect_equal(mean(beta), 2.592, tolerance=1e-2)
  expect_equal(mean(theta$intercept), -1.343559, tolerance=1e-2)


  })
