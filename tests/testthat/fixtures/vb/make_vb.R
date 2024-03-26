#Define specific functions for chosen DE, distribution of parameters, error process
#Differential equation governing dynamics.
DE <- function(y, pars){ #von Bertalanffy growth function
  return(pars[1] * (pars[2] - y))
}

#Normally distributed error
add_error_process <- function(y, sigma_e=0.001){
  return(y + rnorm(length(y), mean=0, sd=sigma_e))
}

#Function to generate distribution of DE parameters
DE_par_generation <- function(n_ind, pars=list(coeff_mean=-3, coeff_sd=1,
                                               max_size_mean=4, max_size_sd=1)){
  par_sample <- data.frame(coeff = exp(rnorm(n_ind, mean = pars[[1]], sd=pars[[2]])),
                           max_size = exp(rnorm(n_ind, mean = pars[[3]], sd=pars[[4]]))+5)
  return(par_sample)
}

#Generate initial conditions
initial_condition_generation <- function(n_ind, pars=list(mean=1, sd=1)){
  y_0_sample <- data.frame(y_0=exp(rnorm(n_ind, mean = pars[[1]], sd=pars[[2]])))
  return(y_0_sample)
}

#Set required values
set.seed(2024) #Guarantees same data each time.
model_name <- "vb"
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

#run models
#Build model fit single ind
set.seed(2024)
vb_data <- readRDS("tests/testthat/fixtures/vb/vb_data_single_ind.rds")
suppressWarnings( #Suppresses stan warnings
  vb_single_ind_test <- rmot_model("vb_single_ind") |>
    rmot_assign_data(step_size = 1.0, #real
                     n_obs = vb_data$n_obs, #integer
                     y_obs = vb_data$y_obs,
                     obs_index = vb_data$obs_index, #vector length N_obs
                     time = vb_data$time, #Vector length N_obs
                     y_0_obs = vb_data$y_0_obs #vector length N_ind
    ) |>
    rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
)
saveRDS(rstan::summary(vb_single_ind_test)$summary,
        file = "tests/testthat/fixtures/vb/vb_baseline_output_single_ind.rds")

#Build model fit multiple inds
set.seed(2024)
vb_data <- readRDS("tests/testthat/fixtures/vb/vb_data_multi_ind.rds")
suppressWarnings( #Suppresses stan warnings
  vb_multi_ind_test <- rmot_model("vb_multi_ind") |>
    rmot_assign_data(step_size = 1.0, #real
                     n_obs = vb_data$n_obs, #integer
                     n_ind = vb_data$n_ind, #integer
                     y_obs = vb_data$y_obs, #vector length N_obs
                     obs_index = vb_data$obs_index, #vector length N_obs
                     time = vb_data$time, #Vector length N_obs
                     ind_id = vb_data$ind_id, #Vector length N_obs
                     y_0_obs = vb_data$y_0_obs #vector length N_ind
    ) |>
    rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
)
saveRDS(rstan::summary(vb_multi_ind_test)$summary,
        file = "tests/testthat/fixtures/vb/vb_baseline_output_multi_ind.rds")

set.seed(Sys.time())
