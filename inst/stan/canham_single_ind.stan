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
  real y_obs[n_obs];
  int obs_index[n_obs];
  real time[n_obs];
  real prior_pars_ind_max_growth[2];
  real prior_pars_ind_size_at_max_growth[2];
  real prior_pars_ind_k[2];
  real prior_pars_global_error_sigma[2];
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0;
  real<lower=0> ind_max_growth;
  real<lower=0> ind_size_at_max_growth;
  real<lower=0> ind_k;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  vector[1] y_temp;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      y_temp[1] = y_hat[i];
      //Estimate next size
      y_hat[i+1] = ode_rk45(DE, y_temp,
        time[i], {time[i+1]},
        ind_max_growth, ind_size_at_max_growth, ind_k)[1][1];
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_max_growth ~lognormal(prior_pars_ind_max_growth[1],
                            prior_pars_ind_max_growth[2]);
  ind_size_at_max_growth ~lognormal(prior_pars_ind_size_at_max_growth[1],
                                    prior_pars_ind_size_at_max_growth[2]);
  ind_k ~lognormal(prior_pars_ind_k[1], prior_pars_ind_k[2]);

  //Global level
  global_error_sigma ~cauchy(prior_pars_global_error_sigma[1],
                             prior_pars_global_error_sigma[2]);
}

generated quantities{
  real y_hat[n_obs];
  vector[1] y_temp;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      y_temp[1] = y_hat[i];

      //Estimate next size
      y_hat[i+1] = ode_rk45(DE, y_temp,
        time[i], {time[i+1]},
        ind_max_growth,
        ind_size_at_max_growth,
        ind_k)[1][1];
    }
  }
}
