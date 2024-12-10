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
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0[n_ind];
  real<lower=0> ind_beta[n_ind];

  real pop_beta_mu;
  real<lower=0> pop_beta_sigma;

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
  ind_beta ~ lognormal(pop_beta_mu,
                    pop_beta_sigma);

  //Population level
  pop_beta_mu ~ normal(0, 1);
  pop_beta_sigma ~cauchy(0, 1);

  //Global level
  global_error_sigma ~cauchy(0, 2);
}

// The output
generated quantities {
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
}
