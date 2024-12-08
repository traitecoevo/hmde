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
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0[n_ind];
  real<lower=0> ind_max_growth[n_ind];
  real<lower=0> ind_size_at_max_growth[n_ind];
  real<lower=0> ind_k[n_ind];

  //Population level
  real pop_max_growth_mean;
  real<lower=0> pop_max_growth_sd;
  real pop_size_at_max_growth_mean;
  real<lower=0> pop_size_at_max_growth_sd;
  real pop_k_mean;
  real<lower=0> pop_k_sd;

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
  ind_max_growth ~lognormal(pop_max_growth_mean,
                            pop_max_growth_sd);
  ind_size_at_max_growth ~lognormal(pop_size_at_max_growth_mean,
                                        pop_size_at_max_growth_sd);
  ind_k ~lognormal(pop_k_mean,
                   pop_k_sd);

  //Species level
  pop_max_growth_mean ~normal(0, 1);
  pop_max_growth_sd ~cauchy(0, 1);
  pop_size_at_max_growth_mean ~normal(0, 1);
  pop_size_at_max_growth_sd ~cauchy(0, 1);
  pop_k_mean ~normal(0, 1);
  pop_k_sd ~cauchy(0, 1);

  //Global level
  global_error_sigma ~cauchy(0, 2);
}

generated quantities{
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
}
