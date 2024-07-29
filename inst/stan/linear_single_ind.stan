//Growth function
functions{
  //Growth function for use with Runge-Kutta method
  //pars = (growth_par, max_size)
  real DE(real y, array[] real pars){ //change number of pars
    return pars[1] - (pars[2] * (y-pars[3])); //growth function
  }

  real rk4_step(real y, array[] real pars, real interval){
    real k1;
    real k2;
    real k3;
    real k4;
    real y_hat;

    k1 = DE(y, pars);
    k2 = DE(y+interval*k1/2.0, pars);
    k3 = DE(y+interval*k2/2.0, pars);
    k4 = DE(y+interval*k3, pars);

    y_hat = y + (1.0/6.0) * (k1 + 2.0*k2 + 2.0*k3 + k4) * interval;

    return y_hat;
  }

  real rk4(real y, array[] real pars, real interval, real step_size){
    int steps;
    real duration;
    real y_hat;
    real step_size_temp;

    duration = 0;
    y_hat = y;

    while(duration < interval){
      //Determine the relevant step size
      step_size_temp = min([step_size, interval-duration]);

      //Get next size estimate
      y_hat = rk4_step(y_hat, pars, step_size_temp);

      //Increment observed duration
      duration = duration + step_size_temp;
    }

    return y_hat;
  }
}

// Data structure
data {
  real step_size;
  int n_obs;
  real y_obs[n_obs];
  int obs_index[n_obs];
  real time[n_obs];
  real y_bar;
  real y_0_obs;
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0;
  real<lower=0> ind_const;
  real<lower=0> ind_beta_1;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  array[3] real pars;

  pars[1] = ind_const;
  pars[2] = ind_beta_1;
  pars[3] = y_bar;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate growth rate
      y_hat[i+1] = rk4(y_hat[i], pars, (time[i+1] - time[i]), step_size);
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_y_0 ~ normal(y_0_obs, global_error_sigma);
  ind_const ~lognormal(0, 5);
  ind_beta_1 ~lognormal(0, 5);

  //Global level
  global_error_sigma ~cauchy(0,1);
}

generated quantities{
  real y_hat[n_obs];
  real Delta_hat[n_obs];
  array[3] real pars;
  real ind_beta_0;

  ind_beta_0 = ind_const + ind_beta_1*y_bar;

  pars[1] = ind_const;
  pars[2] = ind_beta_1;
  pars[3] = y_bar;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate next size
      y_hat[i+1] = rk4(y_hat[i], pars, (time[i+1] - time[i]), step_size);
      Delta_hat[i] = y_hat[i+1] - y_hat[i];

    } else {
      Delta_hat[i] = DE(y_hat[i], pars)*(time[i] - time[i-1]);
    }
  }
}
