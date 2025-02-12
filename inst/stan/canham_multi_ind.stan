//Growth function
functions{
  //pars = (g_max, s_max, k)
  vector DE(real t, vector y, real max_growth, real size_at_max, real k){
    vector[size(y)] dydt = max_growth *
    exp(-0.5 * square(log(y / size_at_max) / k));
    return dydt;
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
  real prior_pars_pop_log_max_growth_mean[2];
  real prior_pars_pop_log_max_growth_sd[2];
  real prior_pars_pop_log_size_at_max_growth_mean[2];
  real prior_pars_pop_log_size_at_max_growth_sd[2];
  real prior_pars_pop_log_k_mean[2];
  real prior_pars_pop_log_k_sd[2];
  real prior_pars_global_error_sigma[2];
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0[n_ind];
  real<lower=0> ind_max_growth[n_ind];
  real<lower=0> ind_size_at_max_growth[n_ind];
  real<lower=0> ind_k[n_ind];

  //Population level
  real pop_log_max_growth_mean;
  real<lower=0> pop_log_max_growth_sd;
  real pop_log_size_at_max_growth_mean;
  real<lower=0> pop_log_size_at_max_growth_sd;
  real pop_log_k_mean;
  real<lower=0> pop_log_k_sd;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  vector[1] y_temp;

  for(i in 1:n_obs){
    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        y_temp[1] = y_hat[i];
        //Estimate next size
        y_hat[i+1] = ode_rk45(DE, y_temp,
          time[i], {time[i+1]},
          ind_max_growth[ind_id[i]],
          ind_size_at_max_growth[ind_id[i]],
          ind_k[ind_id[i]])[1][1];
      }
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_max_growth ~lognormal(pop_log_max_growth_mean,
                            pop_log_max_growth_sd);
  ind_size_at_max_growth ~lognormal(pop_log_size_at_max_growth_mean,
                                        pop_log_size_at_max_growth_sd);
  ind_k ~lognormal(pop_log_k_mean,
                   pop_log_k_sd);

  //Species level
  pop_log_max_growth_mean ~normal(prior_pars_pop_log_max_growth_mean[1],
                              prior_pars_pop_log_max_growth_mean[2]);
  pop_log_max_growth_sd ~cauchy(prior_pars_pop_log_max_growth_sd[1],
                            prior_pars_pop_log_max_growth_sd[2]);
  pop_log_size_at_max_growth_mean ~normal(prior_pars_pop_log_size_at_max_growth_mean[1],
                                      prior_pars_pop_log_size_at_max_growth_mean[2]);
  pop_log_size_at_max_growth_sd ~cauchy(prior_pars_pop_log_size_at_max_growth_sd[1],
                                    prior_pars_pop_log_size_at_max_growth_sd[2]);
  pop_log_k_mean ~normal(prior_pars_pop_log_k_mean[1], prior_pars_pop_log_k_mean[2]);
  pop_log_k_sd ~cauchy(prior_pars_pop_log_k_sd[1], prior_pars_pop_log_k_sd[2]);

  //Global level
  global_error_sigma ~cauchy(prior_pars_global_error_sigma[1],
                             prior_pars_global_error_sigma[2]);
}

generated quantities{
  real y_hat[n_obs];
  vector[1] y_temp;

  //Return the used prior parameters
  real check_prior_pars_pop_log_max_growth_mean[2] = prior_pars_pop_log_max_growth_mean;
  real check_prior_pars_pop_log_max_growth_sd[2] = prior_pars_pop_log_max_growth_sd;
  real check_prior_pars_pop_log_size_at_max_growth_mean[2] = prior_pars_pop_log_size_at_max_growth_mean;
  real check_prior_pars_pop_log_size_at_max_growth_sd[2] = prior_pars_pop_log_size_at_max_growth_sd;
  real check_prior_pars_pop_log_k_mean[2] = prior_pars_pop_log_k_mean;
  real check_prior_pars_pop_log_k_sd[2] = prior_pars_pop_log_k_sd;
  real check_prior_pars_global_error_sigma[2] = prior_pars_global_error_sigma;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        y_temp[1] = y_hat[i];
        //Estimate next size
        y_hat[i+1] = ode_rk45(DE, y_temp,
          time[i], {time[i+1]},
          ind_max_growth[ind_id[i]],
          ind_size_at_max_growth[ind_id[i]],
          ind_k[ind_id[i]])[1][1];
      }
    }
  }
}
