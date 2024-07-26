#Define specific functions for chosen DE, distribution of parameters, error process
#Differential equation governing dynamics.
DE <- function(y, pars){ #Canham function
  return(pars[1] * exp( -0.5 * (log(y / pars[2]) / pars[3])^2 ))
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
  par_sample <- exp(data.frame(mnormt::rmnorm(n_ind,
                                      mean = pars$par_mean_vec,
                                      varcov = pars$par_cov_matrix))
                    )
  names(par_sample) <- c("g_max", "s_max", "k")

  return(par_sample)
}

#Set required values
model_name <- "canham"
set.seed(2024) #Guarantees same data each time.
n_ind <- 3 #Number of individuals for multi-individual data. Single individual takes the first.
n_obs_per_ind <- 10 #How many observations per individual.
interval <- 5 #Time interval between observations
time = seq(from = 0, by = interval, length.out = n_obs_per_ind)

#Produce parameters
DE_pars <- DE_par_generation(n_ind)

initial_conditions <- data.frame(y_0=exp(rnorm(n_ind, mean = 2, sd=1)))
for(i in 1:n_ind){ #Ensure reasonable starting condition based on G. recondita
  while(initial_conditions$y_0[i] <=3 || (initial_conditions$y_0[i] >=9)){
    initial_conditions$y_0[i] <- exp(rnorm(1, mean = 2, sd=1))
  }
}

#Generate true values
true_data <- rmot_build_true_test_data(n_ind, n_obs_per_ind, interval,
                                       DE_pars, initial_conditions, DE)

#Generate observations
y_obs <- rmot_add_normal_error(true_data$y_true)

#Export datasets
rmot_export_test_data(n_obs_per_ind = n_obs_per_ind,
                      n_ind = n_ind,
                      y_obs = y_obs,
                      time = time,
                      DE_pars = DE_pars,
                      initial_conditions = initial_conditions,
                      true_data = true_data,
                      model_name = model_name)
