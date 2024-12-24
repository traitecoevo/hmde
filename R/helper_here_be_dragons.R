#Helper functions for the Here Be Dragons vignette

#Fit finite mixture model with 2 clusters
fit_mix_model <- function(par_est_data, par_names, true_pars){
  possible_error <- par_est_data %>%
    filter(par_names[1] > mean(analysis_data[[par_names[1]]]))

  #To speed up the iterative algorithm we provide some initial conditions
  error_means <- c()
  for(i in 1:length(par_names)){
    error_means[i] <- mean(possible_error[[par_names[i]]])
  }
  mu <- list( #Means from true parameters and extreme estimates
    true = true_pars,
    error = error_means
  )

  mix_model_data <- par_est_data %>%
    select(par_names)

  #Fit multivariate normal finite mixture model to the estimates
  mix_model <- mvnormalmixEM(x = mix_model_data, mu = mu)

  #Extract values
  post_summary_vec <- c()
  for(i in 1:2){
    par_vec_temp <- c()
    for(j in 1:length(par_names)){
      par_vec_temp[j] <- mix_model[[i]]$mu[[i]][j]
    }
    post_summary_vec <- c(post_summary_vec, par_vec_temp)
  }

  #Built data frame to distance between clusters
  dist_table <- tibble()
  for(i in 1:length(par_names)){
    dist_table[[par_names[i]]] <- c(mix_model$mu[[1]][i],
                                    mix_model$mu[[2]][i])
  }

  #Build summary list
  post_summary_list <- list(
    post_summary_vec = post_summary_vec,
    error_prob = step_size_mix_models[[i]]$lambda[2],
    dist = dist(dist_table)
  )

  return(list(
      mix_model = mix_model,
      post_summary_list = post_summary_list
    )
  )
}

#Plotting clusters, 1 and 2 dimensions
plot_mix_model_clusters <- function(mix_model, par_est_data){

}
