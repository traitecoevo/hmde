#Define specific functions for chosen DE, distribution of parameters, error process
#Differential equation governing dynamics.
DE <- function(y, pars){ #Power law function
  return(pars[1] * y^(-pars[2]))
}

#Function to generate distribution of DE parameters
DE_par_generation <- function(n_ind, pars=list(coeff_mean=0, coeff_sd=1,
                                               power_mean=0, power_sd=1)){
  par_sample <- data.frame(coeff = exp(rnorm(n_ind, mean = pars[[1]], sd=pars[[2]])),
                           power = exp(rnorm(n_ind, mean = pars[[1]], sd=pars[[2]])))
  return(par_sample)
}

#Set required values
model_name <- "power"
set.seed(2024)
n_ind <- 3 #Number of individuals for multi-individual data. Single individual takes the first.
n_obs_per_ind <- 7 #How many observations per individual.
interval <- 5 #Time interval between observations
time = seq(from = 0, by = interval, length.out = n_obs_per_ind)

#Produce parameters
DE_pars <- DE_par_generation(n_ind)
initial_conditions <- data.frame(y_0=exp(rnorm(n_ind, mean = 1, sd=1)))

#Generate true values
true_data <- rmot_build_true_test_data(n_ind, n_obs_per_ind, interval,
                                       DE_pars, initial_conditions, DE)

#Generate observations
y_obs <- rmot_add_normal_error(true_data$y_true)

#Export datasets
rmot_export_test_data(n_obs_per_ind,
                      n_ind,
                      y_obs,
                      time,
                      DE_pars,
                      initial_conditions,
                      true_data,
                      model_name)
