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
  real<lower=0> ind_max_growth[n_ind];
  real<lower=0> ind_size_at_max_growth[n_ind];
  real<lower=0> ind_k[n_ind];

  //Species level
  real pop_max_growth_mean;
  real<lower=0> pop_max_growth_sd;
  real pop_size_at_max_growth_mean;
  real<lower=0> pop_size_at_max_growth_sd;
  real pop_k_mean;
  real<lower=0> pop_k_sd;

  //Global level
  real<lower=0> global_error_sigma;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  vector[3] pars;

  for(i in 1:n_obs){
    // Initialise the parameters for the observation
    pars[1] = ind_max_growth[ind_id[i]];
    pars[2] = ind_size_at_max_growth[ind_id[i]];
    pars[3] = ind_k[ind_id[i]];

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        //Estimate next size
        y_hat[i+1] = rk4(y_hat[i], pars, (time[i+1] - time[i]), step_size);
      }
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, global_error_sigma);

  //Priors
  //Individual level
  ind_y_0 ~ normal(y_0_obs, global_error_sigma);
  ind_max_growth ~lognormal(pop_max_growth_mean,
                            pop_max_growth_sd);
  ind_size_at_max_growth ~lognormal(pop_size_at_max_growth_mean,
                                        pop_size_at_max_growth_sd);
  ind_k ~lognormal(pop_k_mean,
                   pop_k_sd);

  //Species level
  pop_max_growth_mean ~normal(0, 1);
  pop_max_growth_sd ~cauchy(0, 1);
  pop_size_at_max_growth_mean ~normal(0, 1);
  pop_size_at_max_growth_sd ~cauchy(0, 1);
  pop_k_mean ~normal(0, 1);
  pop_k_sd ~cauchy(0, 1);

  //Global level
  global_error_sigma ~cauchy(0, 2);
}

generated quantities{
  real y_hat[n_obs];
  real Delta_hat[n_obs];
  vector[3] pars;

  for(i in 1:n_obs){
    // Initialise the parameters for the observation
    pars[1] = ind_max_growth[ind_id[i]];
    pars[2] = ind_size_at_max_growth[ind_id[i]];
    pars[3] = ind_k[ind_id[i]];

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0[ind_id[i]];
    }

    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        //Estimate next size
        y_hat[i+1] = rk4(y_hat[i], pars, (time[i+1] - time[i]), step_size);
        Delta_hat[i] = y_hat[i+1] - y_hat[i];

      } else {
        Delta_hat[i] = DE(y_hat[i], pars)*(time[i] - time[i-1]);
      }
    } else {
      Delta_hat[i] = DE(y_hat[i], pars)*(time[i] - time[i-1]);
    }
  }
}
