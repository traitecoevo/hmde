#Define specific functions for chosen DE, distribution of parameters, error process
#Required library for multivariate normal
require(mnormt)

#Differential equation governing dynamics.
DE <- function(y, pars){ #Canham function
  return(pars[1] * exp( -0.5 * (log(y / pars[2]) / pars[3])^2 ))
}

#Normally distributed error
add_error_process <- function(y, sigma_e=0.001){
  return(y + rnorm(length(y), mean=0, sd=sigma_e))
}

#Function to generate distribution of DE parameters
#Default parameters are based on G. recondita
DE_par_generation <- function(n_ind,
                              pars = list(
                                par_mean_vec = c(-1.76867118,  2.17665912, -0.05967696),
                                par_cov_matrix = matrix(
                                  data = c(0.200199586, 0.003709906, -0.06667686,
                                           0.003709906, 0.035306251, -0.01538283,
                                           -0.066676863 , -0.015382829, 0.12895330),
                                  nrow=3, ncol=3
                                  )
                                )
                              ){
  par_sample <- exp(data.frame(rmnorm(n_ind,
                                      mean = pars$par_mean_vec,
                                      varcov = pars$par_cov_matrix))
                    )
  names(par_sample) <- c("g_max", "s_max", "k")

  return(par_sample)
}

#Generate initial conditions
initial_condition_generation <- function(n_ind,
                                         pars=list(mean=2, sd=1)){
  y_0_sample <- data.frame(y_0=exp(rnorm(n_ind, mean = pars[[1]], sd=pars[[2]])))
  for(i in 1:n_ind){ #Ensure reasonable starting condition based on G. recondita
    while(y_0_sample$y_0[i] <=1 || (y_0_sample$y_0[i] >=15)){
      y_0_sample$y_0[i] <- exp(rnorm(n_ind, mean = pars[[1]], sd=pars[[2]]))
    }
  }
  return(y_0_sample)
}

#Set required values
set.seed(2024) #Guarantees same data each time.
model_name <- "canham"
n_ind <- 3 #Number of individuals for multi-individual data. Single individual takes the first.
n_obs_per_ind <- 7 #How many observations per individual.
interval <- 5 #Time interval between observations
time = seq(from = 0, by = interval, length.out = n_obs_per_ind)

#Produce parameters
DE_pars <- DE_par_generation(n_ind)
initial_conditions <- initial_condition_generation(n_ind)

#Generate true values
true_data <- rmot_build_true_test_data(n_ind, n_obs_per_ind, interval,
                                       DE_pars, initial_conditions, DE)

#Generate observations
y_obs <- add_error_process(true_data$y_true)

#Export data sets
rmot_export_test_data(n_obs_per_ind,
                      n_ind,
                      y_obs,
                      DE_pars,
                      initial_conditions,
                      true_data,
                      model_name)

#run model
#Build model fit single ind
set.seed(2024)
canham_data <- readRDS("tests/testthat/fixtures/canham/canham_data_single_ind.rds")
suppressWarnings( #Suppresses stan warnings
  canham_single_ind_test <- rmot_model("canham_single_ind") |>
    rmot_assign_data(n_obs = canham_data$n_obs, #integer
                     y_obs = canham_data$y_obs,
                     obs_index = canham_data$obs_index, #vector length N_obs
                     time = canham_data$time, #Vector length N_obs
                     y_0_obs = canham_data$y_0_obs #vector length N_ind
    ) |>
    rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
)
saveRDS(rstan::summary(canham_single_ind_test)$summary,
        file = "tests/testthat/fixtures/canham/canham_baseline_output_single_ind.rds")

#Build model fit multiple inds
set.seed(2024)
canham_data <- readRDS("tests/testthat/fixtures/canham/canham_data_multi_ind.rds")
suppressWarnings( #Suppresses stan warnings
  canham_multi_ind_test <- rmot_model("canham_multi_ind") |>
    rmot_assign_data(n_obs = canham_data$n_obs, #integer
                     n_ind = canham_data$n_ind, #integer
                     y_obs = canham_data$y_obs, #vector length N_obs
                     obs_index = canham_data$obs_index, #vector length N_obs
                     time = canham_data$time, #Vector length N_obs
                     ind_id = canham_data$ind_id, #Vector length N_obs
                     y_0_obs = canham_data$y_0_obs #vector length N_ind
    ) |>
    rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
)
saveRDS(rstan::summary(canham_multi_ind_test)$summary,
        file = "tests/testthat/fixtures/canham/canham_baseline_output_multi_ind.rds")

set.seed(Sys.time())

