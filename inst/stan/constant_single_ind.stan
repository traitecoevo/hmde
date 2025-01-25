//Constant DE - Single species
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
  real y_obs[n_obs];
  int obs_index[n_obs];
  real time[n_obs];
  real prior_pars_ind_beta[2];
  real prior_pars_global_error_sigma[2];
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0;
  real<lower=0> ind_beta;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];

  for(i in 1:n_obs){
    //Fits the first size
    if(obs_index[i]==1){
      y_hat[i] = ind_y_0;
    }

    // Estimate next size
    if(i < n_obs){
      y_hat[i+1] = size_step(y_hat[i], ind_beta, (time[i+1]-time[i]));
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_beta ~ lognormal(prior_pars_ind_beta[1],
                       prior_pars_ind_beta[2]);

  //Global level
  global_error_sigma ~cauchy(prior_pars_global_error_sigma[1],
                             prior_pars_global_error_sigma[2]);
}

// The output
generated quantities {
  real y_hat[n_obs];

  for(i in 1:n_obs){
    //Fits the first size
    if(obs_index[i]==1){
      y_hat[i] = ind_y_0;
    }

    // Estimate next size
    if(i < n_obs){
      y_hat[i+1] = size_step(y_hat[i], ind_beta, (time[i+1]-time[i]));
    }
  }
}
