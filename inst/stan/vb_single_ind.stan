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
  real step_size;
  int n_obs;
  real y_obs[n_obs];
  int obs_index[n_obs];
  real time[n_obs];
  real y_0_obs;
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0;
  real<lower=0> ind_y_max;
  real<lower=0> ind_beta;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  array[3] real pars;

  pars[1] = ind_y_max;
  pars[2] = ind_beta;
  pars[3] = ind_y_0;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate growth rate
      y_hat[i+1] = solution(time[i+1], pars);
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_y_0 ~ normal(y_0_obs, global_error_sigma);
  ind_y_max ~lognormal(0, 1);
  ind_beta ~lognormal(0, 1); //Take max obs. size as average value

  //Global level
  global_error_sigma ~cauchy(0,1);
}

generated quantities{
  real y_hat[n_obs];
  real Delta_hat[n_obs];
  array[3] real pars;

  pars[1] = ind_y_max;
  pars[2] = ind_beta;
  pars[3] = ind_y_0;

  real temp_y_final;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate next size
      y_hat[i+1] = solution(time[i+1], pars);
      Delta_hat[i] = y_hat[i+1] - y_hat[i];

    } else { #Estimate next growth based on same time to last.
      temp_y_final = solution(2*time[i] - time[i-1], pars);
      Delta_hat[i] = temp_y_final - y_hat[i];
    }
  }
}
