// Power law model with multiple individuals

functions{
  //Analytic solution to log-transformed power law
  //pars = [ind_coeff, ind_power, ind_y_0]
  real solution(real time, array[] real pars){
    return log(pars[1])/pars[2] +
    (log(pars[3]) - log(pars[1])/pars[2]) * exp((-pars[2] * time));
  }
}

// Data structure
data {
  int n_obs;
  int n_ind;
  real y_obs[n_obs];
  int obs_index[n_obs];
  real time[n_obs];
  int ind_id[n_obs];
  real y_0_obs[n_ind];
  real y_bar;
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0[n_ind];
  real<lower=0> ind_coeff[n_ind];
  real<lower=0> ind_power[n_ind];

  // Population level
  real pop_coeff_mean;
  real<lower=0> pop_coeff_sd;
  real pop_power_mean;
  real<lower=0> pop_power_sd;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  array[3] real pars;

  for(i in 1:n_obs){
    // Initialise the parameters for the observation
    pars[1] = ind_coeff[ind_id[i]];
    pars[2] = ind_power[ind_id[i]];
    pars[3] = ind_y_0[ind_id[i]] - y_bar;

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        //Estimate next size
        y_hat[i+1] = exp(solution(time[i], pars)) + y_bar;
      }
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_y_0 ~ normal(y_0_obs, global_error_sigma);
  ind_coeff ~lognormal(pop_coeff_mean, pop_coeff_sd);
  ind_power ~lognormal(pop_power_mean, pop_power_sd);

  // Population level
  pop_coeff_mean ~normal(0, 2);
  pop_coeff_sd ~cauchy(0, 2);
  pop_power_mean ~normal(0, 2);
  pop_power_sd ~cauchy(0, 2);

  //Global level
  global_error_sigma ~cauchy(0, 5);
}

generated quantities{
  real y_hat[n_obs];
  real Delta_hat[n_obs];
  array[3] real pars;
  real temp_y_final;

  for(i in 1:n_obs){
    // Initialise the parameters for the observation
    pars[1] = ind_coeff[ind_id[i]];
    pars[2] = ind_power[ind_id[i]];
    pars[3] = ind_y_0[ind_id[i]] - y_bar;

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        //Estimate next size
        y_hat[i+1] = solution(time[i+1], pars) + y_bar;
        Delta_hat[i] = y_hat[i+1] - y_hat[i];
      } else { #Estimate next growth based on same time to last.
        temp_y_final = solution(2*time[i] - time[i-1], pars) + y_bar;
        Delta_hat[i] = temp_y_final - y_hat[i];
      }
    } else {
      temp_y_final = solution(2*time[i] - time[i-1], pars) + y_bar;
      Delta_hat[i] = temp_y_final - y_hat[i];
    }
  }
}
