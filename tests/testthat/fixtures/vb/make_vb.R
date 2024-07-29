#Define specific functions for chosen DE, distribution of parameters, error process
#Differential equation governing dynamics.
DE <- function(y, pars){ #Canham function
  return(pars[1] * (pars[2] - y))
}

#Function to generate distribution of DE parameters
#Default parameters are based on G. recondita
DE_par_generation <- function(n_ind,
                              pars = list(
                                par_mean_vec = c(-1, 3.5),
                                par_sd_vec = c(0.1, 0.1)
                              )
                             ){
  par_sample <- exp(
    data.frame(
      growth_par = rnorm(n_ind, mean=pars$par_mean_vec[1], sd = pars$par_sd_vec[1]),
      max_size = rnorm(n_ind, mean=pars$par_mean_vec[2], sd = pars$par_sd_vec[2])
    )
  )
  names(par_sample) <- c("growth_par", "max_size")

  return(par_sample)
}

#Set required values
model_name <- "vb"
set.seed(2024) #Guarantees same data each time.
n_ind <- 3 #Number of individuals for multi-individual data. Single individual takes the first.
n_obs_per_ind <- 20 #How many observations per individual.
interval <- 1 #Time interval between observations
time = seq(from = 0, by = interval, length.out = n_obs_per_ind)

#Produce parameters
DE_pars <- DE_par_generation(n_ind)

initial_conditions <- data.frame(y_0=exp(rnorm(n_ind, mean = 0.5, sd=0.1)))
for(i in 1:n_ind){ #Ensure first size is below the max size parameter
  while(initial_conditions$y_0[i] >= DE_pars$max_size[i]){
    initial_conditions$y_0[i] <- exp(rnorm(1, mean = 2, sd=1))
  }
}

#Generate true values
true_data <- hmDE_build_true_test_data(n_ind, n_obs_per_ind, interval,
                                       DE_pars, initial_conditions, DE)

#Generate observations
y_obs <- hmDE_add_normal_error(true_data$y_true)
y_bar = mean(y_obs)

#Export datasets
hmDE_export_test_data(n_obs_per_ind = n_obs_per_ind,
                      n_ind = n_ind,
                      y_obs = y_obs,
                      time = time,
                      y_bar = y_bar,
                      DE_pars = DE_pars,
                      initial_conditions = initial_conditions,
                      true_data = true_data,
                      model_name = model_name,
                      step_size = 0.1)

