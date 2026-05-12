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
  real y_obs[n_obs];
  int obs_index[n_obs];
  real time[n_obs];
  real prior_pars_ind_a[2];
  real prior_pars_ind_b[2];
  real prior_pars_ind_c[2];
  real prior_pars_global_error_sigma[2];
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0;
  real<lower=0> ind_a;
  real<lower=0> ind_b;
  real<lower=0> ind_c;

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
        ind_a, ind_b, ind_c)[1][1];
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_a ~ lognormal(prior_pars_ind_a[1], prior_pars_ind_a[2]);
  ind_b ~ lognormal(prior_pars_ind_b[1], prior_pars_ind_b[2]);
  ind_c ~ lognormal(prior_pars_ind_c[1], prior_pars_ind_c[2]);

  //Global level
  global_error_sigma ~ cauchy(prior_pars_global_error_sigma[1],
                              prior_pars_global_error_sigma[2]);
}

generated quantities{
  real y_hat[n_obs];
  vector[1] y_temp;

  //Return the used prior parameters
  real check_prior_pars_ind_a[2] = prior_pars_ind_a;
  real check_prior_pars_ind_b[2] = prior_pars_ind_b;
  real check_prior_pars_ind_c[2] = prior_pars_ind_c;
  real check_prior_pars_global_error_sigma[2] = prior_pars_global_error_sigma;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      y_temp[1] = y_hat[i];

      //Estimate next size
      y_hat[i+1] = ode_rk45(DE, y_temp,
        time[i], {time[i+1]},
        ind_a, ind_b, ind_c)[1][1];
    }
  }
}
