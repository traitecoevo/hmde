//Growth function
functions{
  //Analytic solution to von Bertalanffy growth function
  //Pars Y_max, beta, y_0
  real solution(real time, array[] real pars){
    return pars[1] + (pars[3] - pars[1]) * exp(-(pars[2] * time));
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
  real<lower=0> ind_growth_rate[n_ind];
  real<lower=0> ind_max_size[n_ind];

  //Population level
  real pop_growth_rate_mean;
  real<lower=0> pop_growth_rate_sd;
  real pop_max_size_mean;
  real<lower=0> pop_max_size_sd;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  array[3] real pars;

  for(i in 1:n_obs){
    // Initialise the parameters for the observation
    pars[1] = ind_max_size[ind_id[i]] - y_bar;
    pars[2] = ind_growth_rate[ind_id[i]];
    pars[3] = ind_y_0[ind_id[i]] - y_bar;

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        //Estimate next size
        y_hat[i+1] = solution(time[i+1], pars) + y_bar;
      }
    }
  }

  //Likelihood
  y_obs ~normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_y_0 ~normal(y_0_obs, global_error_sigma);
  ind_growth_rate ~lognormal(pop_growth_rate_mean, pop_growth_rate_sd);
  ind_max_size ~lognormal(pop_max_size_mean, pop_max_size_sd);

  //Population level
  pop_growth_rate_mean ~normal(0, 2);
  pop_growth_rate_sd ~cauchy(0, 2);
  pop_max_size_mean ~normal(max(log(y_obs)), 2);
  pop_max_size_sd ~cauchy(0, 2);

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
    pars[1] = ind_max_size[ind_id[i]] - y_bar;
    pars[2] = ind_growth_rate[ind_id[i]];
    pars[3] = ind_y_0[ind_id[i]] - y_bar;

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        //Estimate next size
        y_hat[i+1] = solution(time[i+1], pars) + y_bar;
       Delta_hat[i] = y_hat[i+1] - y_hat[i];
      } else {// Estimate next growth based on same time to last.
        temp_y_final = solution(2*time[i] - time[i-1], pars) + y_bar;
        Delta_hat[i] = temp_y_final - y_hat[i];
      }
    } else {
      temp_y_final = solution(2*time[i] - time[i-1], pars) + y_bar;
      Delta_hat[i] = temp_y_final - y_hat[i];
    }
  }
}
