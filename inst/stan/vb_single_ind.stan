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
  real y_obs[n_obs];
  int obs_index[n_obs];
  real time[n_obs];
  real y_bar;
  real prior_pars_ind_max_size_sd_only;
  real prior_pars_ind_growth_rate[2];
  real prior_pars_global_error_sigma[2];
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0;
  real<lower=0> ind_max_size;
  real<lower=0> ind_growth_rate;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  array[3] real pars;

  pars[1] = ind_max_size - y_bar;
  pars[2] = ind_growth_rate;
  pars[3] = ind_y_0 - y_bar;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate next size
      y_hat[i+1] = solution(time[i+1], pars) + y_bar;
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_max_size ~lognormal(log(max(y_obs)),
                          prior_pars_ind_max_size_sd_only);
  ind_growth_rate ~lognormal(prior_pars_ind_growth_rate[1],
                             prior_pars_ind_growth_rate);

  //Global level
  global_error_sigma ~cauchy(prior_pars_global_error_sigma[1],
                             prior_pars_global_error_sigma[2]);
}

generated quantities{
  real y_hat[n_obs];
  array[3] real pars;

  pars[1] = ind_max_size - y_bar;
  pars[2] = ind_growth_rate;
  pars[3] = ind_y_0 - y_bar;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate next size
      y_hat[i+1] = solution(time[i+1], pars) + y_bar;
    }
  }
}
