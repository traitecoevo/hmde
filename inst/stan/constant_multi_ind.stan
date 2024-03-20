//Constant Growth - Single species
functions{
  //Growth function
  real growth(real beta){
    return beta;
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
  real y_0_obs[n_ind];
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0[n_ind];
  real<lower=0> ind_beta[n_ind];

  real species_beta_mu;
  real<lower=0> species_beta_sigma;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  real Delta_hat[n_obs];

  for(i in 1:n_obs){

    //Fits the first size
    if(obs_index[i]==1){
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    // Estimate growth rate
    Delta_hat[i] = growth(ind_beta[ind_id[i]]);

    // Update next size if relevant
    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        //Analytic solution
        y_hat[i+1] = y_hat[i] + Delta_hat[i]*(time[i+1]-time[i]);
      }
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_y_0 ~ normal(y_0_obs, global_error_sigma);
  ind_beta ~ lognormal(species_beta_mu,
                    species_beta_sigma);

  //Species level
  species_beta_mu ~ normal(0.1, 1);
  species_beta_sigma ~cauchy(0.1, 1);

  //Global level
  global_error_sigma ~cauchy(0.1, 1);
}

// The output
generated quantities {
  real y_hat[n_obs];
  real Delta_hat[n_obs];

  for(i in 1:n_obs){

    //Fits the first size
    if(obs_index[i]==1){
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    // Estimate growth rate
    Delta_hat[i] = growth(ind_beta[ind_id[i]]);

    // Update next size if relevant
    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        //Analytic solution
        y_hat[i+1] = y_hat[i] + Delta_hat[i]*(time[i+1]-time[i]);
      }
    }
  }
}
