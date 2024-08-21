// Power law model with single individual

functions{
  //Analytic solution to log-transformed power law
  //pars = [ind_coeff, ind_power, ind_y_0]
  real solution(real time, array[] real pars){
    return pars[1]/pars[2] +
    (log(pars[3]) - pars[1]/pars[2]) * exp((-pars[2] * time));
  }
}

// Data structure
data {
  int n_obs;
  real y_obs[n_obs];
  int obs_index[n_obs];
  real time[n_obs];
  real y_0_obs;
  real y_bar;
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0;
  real<lower=0> ind_coeff;
  real<lower=0> ind_power;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  array[3] real pars;

  pars[1] = log(ind_coeff);
  pars[2] = ind_power;
  pars[3] = ind_y_0; // / y_bar;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate next size
      y_hat[i+1] = exp(solution(time[i+1], pars)); // * y_bar;
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_y_0 ~ normal(y_0_obs, global_error_sigma);
  ind_coeff ~lognormal(0, 1);
  ind_power ~lognormal(0, 1);

  //Global level
  global_error_sigma ~cauchy(0,5);
}

generated quantities{
  real y_hat[n_obs];
  real Delta_hat[n_obs];
  array[3] real pars;

  pars[1] = log(ind_coeff);
  pars[2] = ind_power;
  pars[3] = ind_y_0; // / y_bar;

  real temp_y_final;
  real test_val;
  test_val = 5;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate next size
      y_hat[i+1] = exp(solution(time[i+1], pars)); // * y_bar;
      Delta_hat[i] = y_hat[i+1] - y_hat[i];

    } else {
      temp_y_final = exp(solution(2*time[i] - time[i-1], pars)); // * y_bar;
      Delta_hat[i] = temp_y_final - y_hat[i];
    }
  }
}
