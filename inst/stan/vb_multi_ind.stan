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
  real y_bar;
  real prior_pars_pop_log_max_size_mean_sd_only;
  real prior_pars_pop_log_max_size_sd[2];
  real prior_pars_pop_log_growth_rate_mean[2];
  real prior_pars_pop_log_growth_rate_sd[2];
  real prior_pars_global_error_sigma[2];
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0[n_ind];
  real<lower=0> ind_growth_rate[n_ind];
  real<lower=0> ind_max_size[n_ind];

  //Population level
  real pop_log_growth_rate_mean;
  real<lower=0> pop_log_growth_rate_sd;
  real pop_log_max_size_mean;
  real<lower=0> pop_log_max_size_sd;

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
  ind_growth_rate ~lognormal(pop_log_growth_rate_mean, pop_log_growth_rate_sd);
  ind_max_size ~lognormal(pop_log_max_size_mean, pop_log_max_size_sd);

  //Population level
  pop_log_max_size_mean ~normal(log(max(y_obs)),
                            prior_pars_pop_log_max_size_mean_sd_only);
  pop_log_max_size_sd ~cauchy(prior_pars_pop_log_max_size_sd[1],
                          prior_pars_pop_log_max_size_sd[2]);
  pop_log_growth_rate_mean ~normal(prior_pars_pop_log_growth_rate_mean[1],
                               prior_pars_pop_log_growth_rate_mean[2]);
  pop_log_growth_rate_sd ~cauchy(prior_pars_pop_log_growth_rate_sd[1],
                             prior_pars_pop_log_growth_rate_sd[2]);

  //Global level
  global_error_sigma ~cauchy(prior_pars_global_error_sigma[1],
                             prior_pars_global_error_sigma[2]);
}

generated quantities{
  real y_hat[n_obs];
  array[3] real pars;

  //Return the used prior parameters
  real check_prior_pars_pop_log_max_size_mean_sd_only = prior_pars_pop_log_max_size_mean_sd_only;
  real check_prior_pars_pop_log_max_size_mean_mean_max_obs = log(max(y_obs));
  real check_prior_pars_pop_log_max_size_sd[2] = prior_pars_pop_log_max_size_sd;
  real check_prior_pars_pop_log_growth_rate_mean[2] = prior_pars_pop_log_growth_rate_mean;
  real check_prior_pars_pop_log_growth_rate_sd[2] = prior_pars_pop_log_growth_rate_sd;
  real check_prior_pars_global_error_sigma[2] = prior_pars_global_error_sigma;

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
}
