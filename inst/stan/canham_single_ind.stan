//Growth function
functions{
  //Growth function for use with Runge-Kutta method
  //pars = (g_max, s_max, k)
  real DE(real y, vector pars){
    return pars[1] *
    exp(-0.5 * pow(log(y / pars[2]) / pars[3], 2));
  }

  real rk4_step(real y, vector pars, real interval){
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

  real rk4(real y, vector pars, real interval, real step_size){
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
  real y_0_obs;
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
  vector[3] pars;

  pars[1] = ind_max_growth;
  pars[2] = ind_size_at_max_growth;
  pars[3] = ind_k;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate next size
      y_hat[i+1] = rk4(y_hat[i], pars, (time[i+1] - time[i]), step_size);
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_y_0 ~ normal(y_0_obs, global_error_sigma);
  ind_max_growth ~lognormal(0, 1);
  ind_size_at_max_growth ~lognormal(3, 1);
  ind_k ~lognormal(0, 1);

  //Global level
  global_error_sigma ~cauchy(0,5);
}

generated quantities{
  real y_hat[n_obs];
  real Delta_hat[n_obs];
  vector[3] pars;

  pars[1] = ind_max_growth;
  pars[2] = ind_size_at_max_growth;
  pars[3] = ind_k;

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
