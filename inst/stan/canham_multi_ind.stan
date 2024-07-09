//Growth function
functions{
  //Growth function for use with Runge-Kutta method
  //pars = (g_max, s_max, k)
  vector DE(real t, vector y, real max_growth, real diameter, real ind_k){
    vector[size(y)] dydt = max_growth *
    exp(-0.5 * square(log(y / diameter) / ind_k));
    return dydt;
  }
  vector concat_arr_vec(array[] vector x) {
    int x_size = size(x);
    vector[x_size] x_tmp;
    for (i in 1:x_size) {
      x_tmp[i] = x[i][1];
    }
    return x_tmp;
  }
/*
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
  */
}

// Data structure
data {
  real step_size;
  int n_obs;
  int n_ind;
  vector[n_obs] y_obs;
  array[n_obs] int obs_index;
  array[n_obs] real time;
  array[n_obs] int ind_id;
  array[n_ind] real y_0_obs;
  array[n_ind, 2] int group_start_stop_idx;
}

// The parameters accepted by the model.
parameters {
  //Individual level
  vector<lower=0>[n_ind] ind_y_0;
  vector<lower=0>[n_ind] ind_max_growth;
  vector<lower=0>[n_ind] ind_diameter_at_max_growth;
  vector<lower=0>[n_ind] ind_k;

  //Species level
  real species_max_growth_mean;
  real<lower=0> species_max_growth_sd;
  real species_diameter_at_max_growth_mean;
  real<lower=0> species_diameter_at_max_growth_sd;
  real species_k_mean;
  real<lower=0> species_k_sd;

  //Global level
  real<lower=0> global_error_sigma;
}

transformed parameters {
  vector[n_obs] mu;
  {
  vector[3] pars;
  for (i in 1:n_ind) {
    // Assuming sorted by tree
    pars[1] = ind_max_growth[i];
    pars[2] = ind_diameter_at_max_growth[i];
    pars[3] = ind_k[i];
    // + 1 to skip initial state
    int start_idx = (group_start_stop_idx[i][1] + 1);
    int end_idx = group_start_stop_idx[i][2];
    // Should you be including data here as a value? Nothing is estimated for y0
    mu[start_idx - 1] = ind_y_0[i];
    mu[start_idx:end_idx] =
      ode_rk45(DE, y_obs[(start_idx - 1):(end_idx - 1)], 0, {1}, pars[1], pars[2], pars[3])[1];
    /*
    for (j in start_idx:end_idx) {
      mu[j] = ode_rk45(DE, [mu[j - 1]]', time[j - 1], {time[j]}, pars[1], pars[2], pars[3])[1][1];
    }
    */
  }
  }
}

// The model to be estimated.
model {

  /*
  for(i in 1:(n_obs - 1)){
    // Initialise the parameters for the observation
    pars[1] = ind_max_growth[ind_id[i]];
    pars[2] = ind_diameter_at_max_growth[ind_id[i]];
    pars[3] = ind_k[ind_id[i]];

    if(obs_index[i]==1){//Fits the first size
      mu[i] = ind_y_0[ind_id[i]];
    }

    if(i < n_obs){
      if(ind_id[i+1]==ind_id[i]){
        //Estimate next size
        real times = (time[i+1] - time[i]);
        mu[i+1] = ode_rk45(DE, [mu[i]]', 0, {times}, pars[1], pars[2], pars[3])[1][1];
      }
    }
  }
  */

  //Likelihood
  y_obs ~ normal(mu, global_error_sigma);

  //Priors
  //Individual level
  ind_y_0 ~ normal(y_0_obs, global_error_sigma);
  ind_max_growth ~lognormal(species_max_growth_mean,
                            species_max_growth_sd);
  ind_diameter_at_max_growth ~lognormal(species_diameter_at_max_growth_mean,
                                        species_diameter_at_max_growth_sd);
  ind_k ~lognormal(species_k_mean,
                   species_k_sd);

  //Species level
  species_max_growth_mean ~normal(0, 1);
  species_max_growth_sd ~cauchy(0, 1);
  species_diameter_at_max_growth_mean ~normal(0, 1);
  species_diameter_at_max_growth_sd ~cauchy(0, 1);
  species_k_mean ~normal(0, 1);
  species_k_sd ~cauchy(0, 1);

  //Global level
  global_error_sigma ~cauchy(0, 2);
}


