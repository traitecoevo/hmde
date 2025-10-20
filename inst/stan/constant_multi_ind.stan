//Constant DE - Single pop
functions{
  //DE function
  real DE(real beta){
    return beta;
  }

  real size_step(real y, real beta, real time){
    return y + DE(beta) * time;
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
  real prior_pars_pop_log_beta_mean[2];
  real prior_pars_pop_log_beta_sd[2];
  real prior_pars_global_error_sigma[2];
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0[n_ind];
  real<lower=0> ind_beta[n_ind];

  real pop_log_beta_mean;
  real<lower=0> pop_log_beta_sd;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];

  for(i in 1:n_obs){
    //Fits the first size
    if(obs_index[i]==1){
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    // Estimate next size
    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        y_hat[i+1] = size_step(y_hat[i], ind_beta[ind_id[i]], (time[i+1]-time[i]));
      }
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_beta ~ lognormal(pop_log_beta_mean,
                    pop_log_beta_sd);

  //Population level
  pop_log_beta_mean ~ normal(prior_pars_pop_log_beta_mean[1],
                       prior_pars_pop_log_beta_mean[2]);
  pop_log_beta_sd ~cauchy(prior_pars_pop_log_beta_sd[1],
                         prior_pars_pop_log_beta_sd[2]);

  //Global level
  global_error_sigma ~cauchy(prior_pars_global_error_sigma[1],
                             prior_pars_global_error_sigma[2]);
}

// The output
generated quantities {
  real y_hat[n_obs];

  //Return the used prior parameters
  real check_prior_pars_pop_log_beta_mean[2];
  real check_prior_pars_pop_log_beta_sd[2];
  real check_prior_pars_global_error_sigma[2];

  check_prior_pars_pop_log_beta_mean = prior_pars_pop_log_beta_mean;
  check_prior_pars_pop_log_beta_sd = prior_pars_pop_log_beta_sd;
  check_prior_pars_global_error_sigma = prior_pars_global_error_sigma;

  for(i in 1:n_obs){

    //Fits the first size
    if(obs_index[i]==1){
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    // Estimate next size
    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        y_hat[i+1] = size_step(y_hat[i], ind_beta[ind_id[i]], (time[i+1]-time[i]));
      }
    }
  }
}
