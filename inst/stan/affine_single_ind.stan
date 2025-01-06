//Growth function
functions{
  //Growth function for use with Runge-Kutta method
  //pars = (beta_0, beta_1)
  real DE_rk4(real y, array[] real pars){ //change number of pars
    return pars[1] - pars[2] * y; //growth function
  }

  real rk4_step(real y, array[] real pars, real interval){
    real k1;
    real k2;
    real k3;
    real k4;
    real y_hat;

    k1 = DE_rk4(y, pars);
    k2 = DE_rk4(y+interval*k1/2.0, pars);
    k3 = DE_rk4(y+interval*k2/2.0, pars);
    k4 = DE_rk4(y+interval*k3, pars);

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

  vector DE_RK45(real t, vector y, real beta_0, real beta_1){
    vector[size(y)] dydt = beta_0 - beta_1 * y;
    return dydt;
  }

  real analytic_solution(real t, real y_0, real beta_0, real beta_1){
    return beta_0/beta_1 + (y_0 - (beta_0/beta_1)) * exp(-beta_1 * t);
  }
}

// Data structure
data {
  real step_size;
  int n_obs;
  real y_obs[n_obs];
  int obs_index[n_obs];
  real time[n_obs];
  int int_method;
  real prior_means[2]; #vector of means for beta parameter priors
  real prior_sds[2]; #Vector of SDs for beta parameter priors
}

// The parameters accepted by the model.
parameters {
  //Individual level
  real<lower=0> ind_y_0;
  real<lower=0> ind_beta_0;
  real<lower=0> ind_beta_1;
}

// The model to be estimated.
model {
  real y_hat[n_obs];
  array[2] real pars;
  vector[1] y_temp;

  pars[1] = ind_beta_0;
  pars[2] = ind_beta_1;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate next size
      if(int_method == 1){ //RK4
        y_hat[i+1] = rk4(y_hat[i], pars, (time[i+1] - time[i]), step_size);
      }

      if(int_method == 2){ //RK45
        y_temp[1] = y_hat[i];
        y_hat[i+1] = ode_rk45(DE_RK45, y_temp,
                      time[i], {time[i+1]},
                      ind_beta_0, ind_beta_1)[1][1];
      }

      if(int_method == 3){ //Analytic solution
        y_hat[i+1] = analytic_solution((time[i+1] - time[1]),
                          ind_y_0,
                          ind_beta_0,
                          ind_beta_1);
      }
    }
  }

  //Likelihood
  y_obs ~ normal(y_hat, 0.1);

  //Priors
  //Individual level
  ind_beta_0 ~lognormal(log(prior_means[1]), prior_sds[1]);
  ind_beta_1 ~lognormal(log(prior_means[2]), prior_sds[2]);
}

generated quantities{
  real y_hat[n_obs];
  array[2] real pars;
  vector[1] y_temp;

  pars[1] = ind_beta_0;
  pars[2] = ind_beta_1;

  for(i in 1:n_obs){

    if(obs_index[i]==1){//Fits the first size
      y_hat[i] = ind_y_0;
    }

    if(i < n_obs){
      //Estimate next size
      if(int_method == 1){ //RK4
        y_hat[i+1] = rk4(y_hat[i], pars, (time[i+1] - time[i]), step_size);
      }

      if(int_method == 2){ //RK45
        y_temp[1] = y_hat[i];
        y_hat[i+1] = ode_rk45(DE_RK45, y_temp,
                      time[i], {time[i+1]},
                      ind_beta_0, ind_beta_1)[1][1];
      }

      if(int_method == 3){ //Analytic solution
        y_hat[i+1] = analytic_solution((time[i+1] - time[1]),
                          ind_y_0,
                          ind_beta_0,
                          ind_beta_1);
      }
    }
  }
}
