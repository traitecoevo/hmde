//Zeide YD growth function (Zeide 1993)
functions{
  //pars = (a, b, c): a = growth at 1-cm, b = expansion factor, c = decline factor
  vector DE(real t, vector y, real a, real b, real c){
    vector[size(y)] dydt = a * exp(b * log(y)) .* exp(-c * (y - 1.0));
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
  real prior_pars_pop_log_a_mean[2];
  real prior_pars_pop_log_a_sd[2];
  real prior_pars_pop_log_b_mean[2];
  real prior_pars_pop_log_b_sd[2];
  real prior_pars_pop_log_c_mean[2];
  real prior_pars_pop_log_c_sd[2];
  real prior_pars_global_error_sigma[2];
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0[n_ind];
  real<lower=0> ind_a[n_ind];
  real<lower=0> ind_b[n_ind];
  real<lower=0> ind_c[n_ind];

  //Population level
  real pop_log_a_mean;
  real<lower=0> pop_log_a_sd;
  real pop_log_b_mean;
  real<lower=0> pop_log_b_sd;
  real pop_log_c_mean;
  real<lower=0> pop_log_c_sd;

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
          ind_a[ind_id[i]],
          ind_b[ind_id[i]],
          ind_c[ind_id[i]])[1][1];
      }
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_a ~ lognormal(pop_log_a_mean, pop_log_a_sd);
  ind_b ~ lognormal(pop_log_b_mean, pop_log_b_sd);
  ind_c ~ lognormal(pop_log_c_mean, pop_log_c_sd);

  //Population level
  pop_log_a_mean ~ normal(prior_pars_pop_log_a_mean[1],
                          prior_pars_pop_log_a_mean[2]);
  pop_log_a_sd ~ cauchy(prior_pars_pop_log_a_sd[1],
                        prior_pars_pop_log_a_sd[2]);
  pop_log_b_mean ~ normal(prior_pars_pop_log_b_mean[1],
                          prior_pars_pop_log_b_mean[2]);
  pop_log_b_sd ~ cauchy(prior_pars_pop_log_b_sd[1],
                        prior_pars_pop_log_b_sd[2]);
  pop_log_c_mean ~ normal(prior_pars_pop_log_c_mean[1],
                          prior_pars_pop_log_c_mean[2]);
  pop_log_c_sd ~ cauchy(prior_pars_pop_log_c_sd[1],
                        prior_pars_pop_log_c_sd[2]);

  //Global level
  global_error_sigma ~ cauchy(prior_pars_global_error_sigma[1],
                              prior_pars_global_error_sigma[2]);
}

generated quantities{
  real y_hat[n_obs];
  vector[1] y_temp;

  //Return the used prior parameters
  real check_prior_pars_pop_log_a_mean[2] = prior_pars_pop_log_a_mean;
  real check_prior_pars_pop_log_a_sd[2] = prior_pars_pop_log_a_sd;
  real check_prior_pars_pop_log_b_mean[2] = prior_pars_pop_log_b_mean;
  real check_prior_pars_pop_log_b_sd[2] = prior_pars_pop_log_b_sd;
  real check_prior_pars_pop_log_c_mean[2] = prior_pars_pop_log_c_mean;
  real check_prior_pars_pop_log_c_sd[2] = prior_pars_pop_log_c_sd;
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
          ind_a[ind_id[i]],
          ind_b[ind_id[i]],
          ind_c[ind_id[i]])[1][1];
      }
    }
  }
}
