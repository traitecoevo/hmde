#Generates data for constant growth model testing
#Differential equation governing dynamics.
DE <- function(y, pars){
  return(pars[1])
}

#Normally distributed error
add_error_process <- function(y, sigma_e=0.001){
  return(y + rnorm(length(y), mean=0, sd=sigma_e))
}

#Function to generate distribution of DE parameters
DE_par_generation <- function(n_ind, pars=list(mean=0, sd=1)){
  par_sample <- data.frame(beta=exp(rnorm(n_ind, mean = pars[[1]], sd=pars[[2]])))
  return(par_sample)
}

#Generate initial conditions
initial_condition_generation <- function(n_ind, pars=list(mean=2, sd=1)){
  y_0_sample <- data.frame(y_0=exp(rnorm(n_ind, mean = pars[[1]], sd=pars[[2]])))
  return(y_0_sample)
}

#Set required values
set.seed(2024) #Guarantees same data each time.
model_name <- "constant"
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

#Export datasets
rmot_export_test_data(n_obs_per_ind,
                      n_ind,
                      y_obs,
                      DE_pars,
                      initial_conditions,
                      true_data,
                      model_name)

#Build model fit single ind
set.seed(2024)
const_data <- readRDS("tests/testthat/fixtures/constant/constant_data_single_ind.rds")
suppressWarnings( #Suppresses stan warnings
  constant_single_ind_test <- rmot_model("constant_single_ind") |>
    rmot_assign_data(n_obs = const_data$n_obs, #integer
                     y_obs = const_data$y_obs,
                     obs_index = const_data$obs_index, #vector length N_obs
                     time = const_data$time, #Vector length N_obs
                     y_0_obs = const_data$y_0_obs #vector length N_ind
    ) |>
    rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
)
saveRDS(rstan::summary(constant_single_ind_test)$summary,
        file = "tests/testthat/fixtures/constant/constant_baseline_output_single_ind.rds")

#Build model fit multiple inds
set.seed(2024)
const_data <- readRDS("tests/testthat/fixtures/constant/constant_data_multi_ind.rds")
suppressWarnings( #Suppresses stan warnings
  constant_multi_ind_test <- rmot_model("constant_multi_ind") |>
    rmot_assign_data(n_obs = const_data$n_obs, #integer
                     n_ind = const_data$n_ind, #integer
                     y_obs = const_data$y_obs, #vector length N_obs
                     obs_index = const_data$obs_index, #vector length N_obs
                     time = const_data$time, #Vector length N_obs
                     ind_id = const_data$ind_id, #Vector length N_obs
                     y_0_obs = const_data$y_0_obs #vector length N_ind
    ) |>
    rmot_run(chains = 1, iter = 300, verbose = FALSE, show_messages = FALSE)
)
saveRDS(rstan::summary(constant_multi_ind_test)$summary,
        file = "tests/testthat/fixtures/constant/constant_baseline_output_multi_ind.rds")

set.seed(Sys.time())
