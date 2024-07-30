#General functions for data generation
#Runge-Kutta 4th order
hmde_rk4_est <- function(y_0, DE, pars, step_size, n_step){
  runge_kutta_int <- c(y_0)
  for(i in 2:n_step){
    k1 <- DE(runge_kutta_int[i-1], pars)
    k2 <- DE((runge_kutta_int[i-1] + step_size*k1/2), pars)
    k3 <- DE((runge_kutta_int[i-1] + step_size*k2/2), pars)
    k4 <- DE((runge_kutta_int[i-1] + step_size*k3), pars)

    runge_kutta_int[i] <- runge_kutta_int[i-1] + (1/6)*(k1 + 2*k2 + 2*k3 + k4)*step_size
  }
  return(runge_kutta_int)
}

hmde_build_true_test_data <- function(n_ind, n_obs, interval,
                                      DE_pars, initial_conditions, DE){
  time <- seq(from = 0, by = interval, length.out = n_obs)

  true_data <- data.frame()
  for(i in 1:n_ind){
    #Use RK4 with small step size to project forward from initial size
    runge_kutta_int <- hmde_rk4_est(y_0 = initial_conditions$y_0[i],
                                    DE = DE,
                                    pars = DE_pars[i,],
                                    step_size = 0.1,
                                    n_step = (1 + n_obs*interval/0.1))

    #Take a subset of the estimates which are in line with survey structure
    runge_kutta_survey <- runge_kutta_int[seq(from=1,
                                              to = length(runge_kutta_int),
                                              by = (length(runge_kutta_int)/n_obs))]

    data_temp <- data.frame( #Build data frame
      y_true = as.numeric(runge_kutta_survey),
      time = time,
      ind_id = rep(i, times=n_obs)
    )

    #Concatenate data
    true_data <- rbind(true_data, data_temp)
  }

  return(true_data)
}

hmde_add_normal_error <- function(y, sigma_e=0.001){
  return(y + rnorm(length(y), mean=0, sd=sigma_e))
}

#Save data to files
hmde_export_test_data <- function(n_obs_per_ind,
                                  n_ind,
                                  y_obs,
                                  time,
                                  y_bar = NULL,
                                  DE_pars,
                                  initial_conditions,
                                  true_data,
                                  model_name,
                                  step_size = 1){

  if(! is.null(y_bar)){ # Power law and vB require y_bar
    single_ind_data <- list(
      step_size = step_size, #Number for model RK4 alg
      n_obs = n_obs_per_ind, #Number
      y_obs = y_obs[1:n_obs_per_ind], #Vector indexed by n_obs
      obs_index = 1:n_obs_per_ind, #Vector indexed by n_obs
      time = time, #Vector indexed by n_obs
      y_0_obs = y_obs[1], #Number
      y_bar = y_bar, #Real
      n_pars = ncol(DE_pars), #Number
      single_true_data = list(
        DE_pars = DE_pars[1,],
        initial_conditions = initial_conditions[1,],
        ind_id = 1,
        true_data = true_data[1:n_obs_per_ind,]
      )
    )

    multi_ind_data <- list(
      step_size = 1, #Number
      n_obs = length(y_obs), #Number
      n_ind = n_ind, #Number
      y_obs = y_obs, #Vector indexed by n_obs
      obs_index = rep(1:n_obs_per_ind, times = n_ind), #Vector indexed by n_obs
      time = rep(time, times=n_ind), #Vector indexed by n_obs
      ind_id = sort(rep(1:n_ind, times = n_obs_per_ind)), #Vector indexed by n_obs
      y_0_obs = y_obs[seq(from = 1, to=n_ind*n_obs_per_ind, by=n_obs_per_ind)], #Vector indexed by n_ind
      y_bar = y_bar, #Real
      n_pars = ncol(DE_pars),
      multi_true_data = list(
        DE_pars = DE_pars,
        initial_conditions = initial_conditions,
        ind_id = c(1:n_ind),
        true_data = true_data
      )
    )

  } else { # Constant and Canham do not require y_bar
    single_ind_data <- list(
      step_size = step_size, #Number for model RK4 alg
      n_obs = n_obs_per_ind, #Number
      y_obs = y_obs[1:n_obs_per_ind], #Vector indexed by n_obs
      obs_index = 1:n_obs_per_ind, #Vector indexed by n_obs
      time = time, #Vector indexed by n_obs
      y_0_obs = y_obs[1], #Number
      n_pars = ncol(DE_pars), #Number
      single_true_data = list(
        DE_pars = DE_pars[1,],
        initial_conditions = initial_conditions[1,],
        ind_id = 1,
        true_data = true_data[1:n_obs_per_ind,]
      )
    )

    multi_ind_data <- list(
      step_size = 1, #Number
      n_obs = length(y_obs), #Number
      n_ind = n_ind, #Number
      y_obs = y_obs, #Vector indexed by n_obs
      obs_index = rep(1:n_obs_per_ind, times = n_ind), #Vector indexed by n_obs
      time = rep(time, times=n_ind), #Vector indexed by n_obs
      ind_id = sort(rep(1:n_ind, times = n_obs_per_ind)), #Vector indexed by n_obs
      y_0_obs = y_obs[seq(from = 1, to=n_ind*n_obs_per_ind, by=n_obs_per_ind)], #Vector indexed by n_ind
      n_pars = ncol(DE_pars),
      multi_true_data = list(
        DE_pars = DE_pars,
        initial_conditions = initial_conditions,
        ind_id = c(1:n_ind),
        true_data = true_data
      )
    )
  }

  if(! dir.exists(test_path("fixtures", model_name))) dir.create(test_path("fixtures", model_name))
  filename <- paste("tests/testthat/fixtures", "/", model_name, "/", model_name, "_data", sep="")
  saveRDS(single_ind_data, file=paste(filename, "single_ind.rds", sep="_"))
  saveRDS(multi_ind_data, file=paste(filename, "multi_ind.rds", sep="_"))
}
