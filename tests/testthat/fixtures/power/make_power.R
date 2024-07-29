#Define specific functions for chosen DE, distribution of parameters, error process
#Differential equation governing dynamics.
DE <- function(y, pars){ #Power law function
  return(pars[1] * (y)^(-pars[2]))
}

#Function to generate distribution of DE parameters
DE_par_generation <- function(n_ind, pars=list(coeff_mean=0.5, coeff_sd=1,
                                               power_mean=-1, power_sd=0.5)){
  par_sample <- data.frame(coeff = exp(rnorm(n_ind, mean = pars[[1]], sd=pars[[2]])),
                           power = exp(rnorm(n_ind, mean = pars[[3]], sd=pars[[4]])))
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
initial_conditions <- data.frame(y_0=exp(rnorm(n_ind, mean = 2, sd=1)))

#Generate true values
true_data <- hmDE_build_true_test_data(n_ind, n_obs_per_ind, interval,
                                       DE_pars, initial_conditions, DE)

#Generate observations
y_obs <- hmDE_add_normal_error(true_data$y_true)
y_bar <- mean(y_obs)

#Export datasets
hmDE_export_test_data(n_obs_per_ind = n_obs_per_ind,
                      n_ind = n_ind,
                      y_obs = y_obs,
                      time = time,
                      y_bar = y_bar,
                      DE_pars = DE_pars,
                      initial_conditions = initial_conditions,
                      true_data = true_data,
                      model_name = model_name)
