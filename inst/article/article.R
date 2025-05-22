#Code to replicate results in hmde paper
# install.packages("remotes")
#remotes::install_github("traitecoevo/hmde@a79baae6c2df22789cccc1fd96eb3a55da9628ec")

{
  library(hmde)
  library(ggplot2)
  library(cowplot)
  library(tidyverse)
  library(rstan)
  library(deSolve)
}


#-----------------------------------------------------------------------------#
# Figure 1
set.seed(2025)
{
#Analytic solution for vB model
solution <- function(t, pars = list(y_0, beta, S_max)){
  return(
    pars[[3]] + (pars[[1]] - pars[[3]])*exp(-t * pars[[2]])
  )
}

#Set up 'observation' data
pars_1 <- list(
  y_0 = 1,
  beta = 0.5,
  S_max = 7
)
pars_2 <- list(
  y_0 = 1,
  beta = 0.8,
  S_max = 12
)
obs_time <- 0:9

obs_data <- tibble(
  ind_id = c(
    rep(1, times = length(obs_time)),
    rep(2, times = length(obs_time))
  ),
  y_true = c(
    solution(obs_time, pars = pars_1),
    solution(obs_time, pars = pars_2)
  ),
  time = c(obs_time, obs_time)
) %>%
  mutate(y_obs = y_true + rnorm((length(obs_time)*2), 0, 0.2),
         obs_index = time + 1)

obs_plot <-
  ggplot(data=obs_data, aes(group = ind_id)) +
  geom_point(aes(x = time, y=y_obs,
                 colour = as.factor(ind_id),
                 shape = as.factor(ind_id)),
             size = 2, stroke = 1
  ) +
  geom_line(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
            linetype = "dashed", alpha = 0.5) +
  scale_shape_manual(values = c(1,3)) +
  labs(title = "Repeat observations over time", x="Time", y="Observed size", colour="Individual",
       shape = "Individual", linetype = "Individual") +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.2),
        axis.ticks = element_blank(),
        axis.text = element_blank())

#Fit model
data <- tibble(
  ind_id = obs_data$ind_id,
  time = obs_data$time,
  y_obs = obs_data$y_obs,
  obs_index = obs_data$obs_index
)

fit <-
  hmde_model("vb_multi_ind") |>
  hmde_assign_data(data) |>
  hmde_run(chains = 4,
           cores = 4,
           iter = 2000)

#Diag plots
diag_plot <- traceplot(fit, pars=c("ind_max_size", "ind_growth_rate"), inc_warmup=FALSE)
diag_plot

#plots of sizes over time
ests <- hmde_extract_estimates(fit, data)

fit_plot <-
  ggplot(data=ests$measurement_data, aes(group = ind_id)) +
  geom_point(aes(x = time, y=y_obs,
                 colour = as.factor(ind_id),
                 shape = as.factor(ind_id)),
             size = 2, stroke = 1
  ) +
  geom_line(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
            linetype = "dashed", alpha = 0.5) +
  geom_line(aes(x = time, y=y_hat),
            colour = "black", alpha = 0.5,
            linetype = "solid") +
  geom_point(aes(x = time, y=y_hat), colour = "black",
             shape = 2, size = 2, stroke = 1, alpha = 0.8) +
  scale_shape_manual(values = c(1,3)) +
  labs(x="Time", y="Observed size", colour="Individual",
       shape = "Individual", linetype = "Individual",
       title = "Observed and estimated sizes over time") +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.2),
        axis.ticks = element_blank(),
        axis.text = element_blank())
fit_plot

fitted_function_plot <- hmde_plot_de_pieces(estimate_list = ests,
                                            xlab = "Size",
                                            ylab = "Growth rate",
                                            title = "Fitted growth functions",
                                            colour = "black",
                                            alpha = 0.8) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
fitted_function_plot

#Theoretical size over time plots
theory_pars_list <- list(
  const_pars = list(beta = 1.5),
  vb_pars = list(beta = 0.3, S_max = 15),
  canham_pars = list(g_max = 0.8,
                     y_max = 8,
                     k = 1)
)
y_0 = 1

times <- seq(from = 0, to = 50, by = 0.01)

#Constant
const_size_time_data <- tibble(
  time = times[c(1, length(times))]
) %>%
  mutate(y_true = y_0 + theory_pars_list$const_pars[[1]] * time)
const_size_time_plot <-
  ggplot(const_size_time_data, aes(x = time, y = y_true)) +
  geom_line(colour = "black", linewidth = 1) +
  labs(x="Time", y="Size") +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

#vB
vb_size_time_plot <-
  ggplot() +
  geom_function(fun=solution,
                args=list(pars = c(y_0 = 1,
                                   theory_pars_list$vb_pars)),
                colour="black", linewidth=1, alpha = 1,
                xlim=c(0,20)) +
  labs(x="Time", y="Size") +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

#Canham
#Build data
Canham_DE <- function(Time, State, Pars) { #Pars: g_max, y_max, k
  with(as.list(c(State, Pars)), {
    dY <- g_max * exp(-0.5 * (log(Y / y_max) / k)^2)

    return(list(c(dY)))
  })
}

pars_combo <- c(g_max = 0.8,
                y_max = 8,
                k = 1)
times <- seq(0, 50, by = 0.001)
yini  <- c(Y = 1) #Initial condition
canham_true_y <- ode(yini, times, Canham_DE, pars_combo, method = "rk4")[,2]
canham_true_data <- tibble(
  y = canham_true_y,
  time = times
)

canham_size_time_plot <-
  ggplot(canham_true_data, aes(x = time, y = y)) +
  geom_line(colour = "black", linewidth = 1) +
  labs(x="Time", y="Size") +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

#Get size limits
y_0_vec <- c(
  const_size_time_data$y_true[1],
  1,
  canham_true_data$y[1]
)
y_final_vec <- c(
  max(const_size_time_data$y_true),
  theory_pars_list$vb_pars$S_max,
  max(canham_true_data$y)
)

#Function plots
function_plots <- list()
de_name <- c("constant_single_ind",
             "vb_single_ind",
             "canham_single_ind")
for(i in 1:3){
  DE_function <- hmde_model_des(de_name[i])
  args_list <- list(
    pars = theory_pars_list[[i]]
  )
  function_plots[[i]] <-
    ggplot() +
    xlim((y_0_vec[i]-1), (y_final_vec[i]+1)) +
    labs(x = "Size", y = "Growth rate") +
    geom_function(fun=DE_function, args=args_list,
                  colour="black", linewidth=1, alpha = 1,
                  xlim=c(y_0_vec[i], y_final_vec[i])) +
    theme_classic()+
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())
}

function_selection <- plot_grid(
  function_plots[[1]], const_size_time_plot,
  function_plots[[2]], vb_size_time_plot,
  function_plots[[3]], canham_size_time_plot,
  nrow = 3,
  byrow = TRUE,
  align = "hv"
)
function_selection

#Produces the plots for Figure 1, excluding the screenshot of code
fig_1_pieces <- plot_grid(
  obs_plot,
  function_selection,
  diag_plot,
  fitted_function_plot,
  fit_plot,
  nrow = 3,
  byrow = TRUE
)

print(fig_1_pieces)
}

#-----------------------------------------------------------------------------#
# Constant function demonstration
set.seed(2025)
{
  histogram_func <- function(data, var, main, xlab, ...){
    ggplot2::ggplot(data = data, aes(x = {{var}})) +
      geom_histogram(colour = "black", fill = "lightblue", ...) +
      labs(title = main,
           x= xlab) +
      theme_classic()
  }

trout_constant_fit <-
  hmde_model("constant_multi_ind") |>
  hmde_assign_data(data = Trout_Size_Data)  |>
  hmde_run(chains = 4, cores = 4, iter = 2000)

#Diag plots, requires large image size to see properly
diag_plot_growth_rate <- traceplot(trout_constant_fit,
                                    pars="ind_beta",
                                    inc_warmup=FALSE)
print(diag_plot_growth_rate)

trout_estimates <-
  hmde_extract_estimates(trout_constant_fit,
                         input_measurement_data = Trout_Size_Data)

trout_estimates

#relationship between estimate and observed sizes
cor(trout_estimates$measurement_data$y_obs, trout_estimates$measurement_data$y_hat)^2
r_sq_est <- cor(trout_estimates$measurement_data$y_obs,
                trout_estimates$measurement_data$y_hat)^2
r_sq <- paste0("R^2 = ",
               signif(r_sq_est,
                      digits = 3))

obs_est_size_plot <- ggplot(data = trout_estimates$measurement_data,
                            aes(x = y_obs, y = y_hat)) +
  geom_point(shape = 16, size = 1, colour = "green4") +
  xlab("Y obs.") +
  ylab("Y est.") +
  geom_abline(slope = 1, linetype = "dashed") +
  annotate("text", x = 45, y = 80,
           label = r_sq) +
  theme_classic()

#Plots of size over time for a sample of 5 individuals
size_over_time_plot <- hmde_plot_obs_est_inds(n_ind_to_plot = 5,
                                              measurement_data =
                                                trout_estimates$measurement_data) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.3)) +
  guides(colour=guide_legend(ncol=2))

ind_hist_beta <- histogram_func(trout_estimates$individual_data,
                               ind_beta_mean,
                               main = "Individual beta parameters",
                               xlab = "beta estimate")

de_pieces <- hmde_plot_de_pieces(trout_estimates)

#Figure 2
fig_2 <- plot_grid(
  obs_est_size_plot,
  size_over_time_plot,
  de_pieces,
  ind_hist_beta,
  nrow = 2,
  byrow = TRUE,
  labels = c("(a)", "(b)",
             "(c)", "(d)")
)

print(fig_2)

#Table 1: species-level hyper parameters
exp_mean <- trout_estimates$population_data[1,c(1,2,4,5)] %>%
  mutate(mean = exp(mean),
         CI_lower = exp(CI_lower),
         CI_upper = exp(CI_upper),
         par_name = "pop_beta_mu")
sp_ests <- rbind(trout_estimates$population_data[,c(1,2,4,5)],
      exp_mean)

print(sp_ests)
}

#-----------------------------------------------------------------------------#
# von Bertalanffy function demonstration
set.seed(2025)
{
  lizard_vb_fit <- hmde_model("vb_multi_ind") |>
    hmde_assign_data(data = Lizard_Size_Data)  |>
    hmde_run(chains = 4, cores = 4, iter = 2000)

  diag_max_size <- traceplot(lizard_vb_fit,
            pars="ind_max_size",
            inc_warmup=FALSE)
  diag_growth_rate <- traceplot(lizard_vb_fit,
                             pars="ind_growth_rate",
                             inc_warmup=FALSE)
  print(diag_max_size)
  print(diag_growth_rate)

  lizard_estimates <- hmde_extract_estimates(fit = lizard_vb_fit,
                                             input_measurement_data = Lizard_Size_Data)


  #Quantitative R^2
  r_sq_est <- cor(lizard_estimates$measurement_data$y_obs,
                  lizard_estimates$measurement_data$y_hat)^2
  r_sq <- paste0("R^2 = ",
                 signif(r_sq_est,
                        digits = 3))

  obs_scatter <- ggplot(data = lizard_estimates$measurement_data,
                        aes(x = y_obs, y = y_hat)) +
    geom_point(shape = 16, size = 1, colour = "green4") +
    xlab("Y obs.") +
    ylab("Y est.") +
    geom_abline(slope = 1, linetype = "dashed") +
    annotate("text", x = 25, y = 18,
             label = r_sq) +
    theme_classic()

  #Plots of size over time for a sample of 5 individuals
  obs_est_ind <- hmde_plot_obs_est_inds(n_ind_to_plot = 5,
                                        measurement_data =
                                          lizard_estimates$measurement_data) +
    theme(legend.position = "inside",
          legend.position.inside = c(0.8, 0.2))

  #1-dimensional parameter distributions
  s_max_hist <- ggplot(lizard_estimates$individual_data,
                       aes(ind_max_size_mean)) +
    geom_histogram(bins = 10,
                   colour = "black",
                   fill = "lightblue") +
    labs(x="S_max estimate") +
    theme_classic()

  beta_hist <- ggplot(lizard_estimates$individual_data,
                      aes(ind_growth_rate_mean)) +
    geom_histogram(bins = 10,
                   colour = "black",
                   fill = "lightblue") +
    labs(x="beta estimate") +
    theme_classic()

  #2-dimensional parameter distribution
  par_scatter <- ggplot(data = lizard_estimates$individual_data,
                        aes(x = ind_max_size_mean, y = ind_growth_rate_mean)) +
    geom_point(shape = 16, size = 1, colour = "green4") +
    xlab("Individual max sizes (mm)") +
    ylab("Individual growth rate parameters") +
    theme_classic()

  #Correlation of parameters
  cor(lizard_estimates$individual_data$ind_max_size_mean,
      lizard_estimates$individual_data$ind_growth_rate_mean,
      method = "spearman")

  #Plot function pieces over estimated sizes.
  de_pieces <- hmde_plot_de_pieces(lizard_estimates)

  #Figure 3
  fig_3 <- plot_grid(
    obs_scatter,
    obs_est_ind,
    de_pieces,
    par_scatter,
    s_max_hist,
    beta_hist,
    nrow = 3,
    byrow = TRUE,
    labels = c("(a)", "(b)",
               "(c)", "(d)",
               "(e)", "(f)")
  )
  print(fig_3)

  exp_mean <- lizard_estimates$population_data[c(1,3),c(1,2,4,5)] %>%
    mutate(mean = exp(mean),
           CI_lower = exp(CI_lower),
           CI_upper = exp(CI_upper))
  exp_mean$par_name <- c("pop_max_size_mean", "pop_growth_rate_mean")
  sp_pars <- rbind(lizard_estimates$population_data[,c(1,2,4,5)],
        exp_mean)

  print(sp_pars)
}


#-----------------------------------------------------------------------------#
# Canham function demonstration
# Due to the runtime we skip fitting the model and use provided model fit in Tree_Size_Ests object
if(FALSE){ #Re-run model to reproduce Tree_Size_Ests results
  set.seed(2025)
  tree_fit <-
    hmde_model("canham_multi_ind") |>
    hmde_assign_data(data = Tree_Size_Data)  |>
    hmde_run(chains = 4, cores = 4, iter = 2000)

  Tree_Size_Ests <-
    hmde_extract_estimates(
      fit = tree_fit,
      input_measurement_data = Tree_Size_Data)
}

set.seed(2025)
{
  #Quantitative R^2
  r_sq_est <- cor(Tree_Size_Ests$measurement_data$y_obs,
                  Tree_Size_Ests$measurement_data$y_hat)^2
  r_sq <- paste0("R^2 = ",
                 signif(r_sq_est,
                        digits = 3))

  obs_est_scatter <- ggplot(data = Tree_Size_Ests$measurement_data,
                            aes(x = y_obs, y = y_hat)) +
    geom_point(shape = 16, size = 1, colour = "green4") +
    xlab("Y obs.") +
    ylab("Y est.") +
    geom_abline(slope = 1, linetype = "dashed") +
    annotate("text", x = 7, y = 22,
             label = r_sq) +
    theme_classic()

  #Plots of size over time for a sample of 5 individuals
  obs_est_size_plot <- hmde_plot_obs_est_inds(n_ind_to_plot = 5,
                                              measurement_data = Tree_Size_Ests$measurement_data) +
    theme(legend.position = "top") +
    guides(colour=guide_legend(nrow=2,byrow=TRUE))

  #1-dimensional parameter distributions
  gmax_hist <- ggplot(Tree_Size_Ests$individual_data,
         aes(ind_max_growth_mean)) +
    geom_histogram(bins = 10,
                   colour = "black",
                   fill = "lightblue") +
    labs(x="g_max estimate") +
    theme_classic()

  Smax_hist <- ggplot(Tree_Size_Ests$individual_data,
         aes(ind_size_at_max_growth_mean)) +
    geom_histogram(bins = 10,
                   colour = "black",
                   fill = "lightblue") +
    labs(x="S_max estimate") +
    theme_classic()

  k_hist <- ggplot(Tree_Size_Ests$individual_data,
         aes(ind_k_mean)) +
    geom_histogram(bins = 10,
                   colour = "black",
                   fill = "lightblue") +
    labs(x="k estimate") +
    theme_classic()

  #2-dimensional parameter distributions
  pairplot1 <- ggplot(data = Tree_Size_Ests$individual_data,
                      aes(x = ind_max_growth_mean, y = ind_size_at_max_growth_mean)) +
    geom_point(shape = 16, size = 1, colour = "green4") +
    xlab("Ind. max growth (cm/yr)") +
    ylab("Ind. size at max growth (cm)") +
    theme_classic()

  pairplot2 <- ggplot(data = Tree_Size_Ests$individual_data,
                      aes(x = ind_k_mean, y = ind_max_growth_mean)) +
    geom_point(shape = 16, size = 1, colour = "green4") +
    ylab("Ind. max growth (cm/yr)") +
    xlab("Ind. spread par.") +
    theme_classic()

  pairplot3 <- ggplot(data = Tree_Size_Ests$individual_data,
                      aes(x = ind_size_at_max_growth_mean, y = ind_k_mean)) +
    geom_point(shape = 16, size = 1, colour = "green4") +
    ylab("Ind. spread par.") +
    xlab("Ind. size at max growth (cm)") +
    theme_classic()

  est_de_plot <- hmde_plot_de_pieces(Tree_Size_Ests)

  #Figure 4
  piece_1 <- plot_grid(
    obs_est_scatter,
    obs_est_size_plot,
    est_de_plot,
    nrow = 1,
    byrow = TRUE,
    labels = c("(a)", "(b)",
               "(c)")
  )
  piece_2 <-
  plot_grid(
    gmax_hist,
    Smax_hist,
    k_hist,
    pairplot1,
    pairplot3,
    pairplot2,
    nrow = 2,
    byrow = TRUE,
    labels = c("(d)",
               "(e)", "(f)",
               "(g)", "(h)",
               "(i)"),
    align = "hv"
  )

  fig_4 <- plot_grid(piece_1, piece_2, nrow = 2,
            rel_heights = c(1/3,2/3))
  print(fig_4)

  exp_mean <- Tree_Size_Ests$population_data[c(1,3,5),c(1,2,4,5)] %>%
    mutate(mean = exp(mean),
           CI_lower = exp(CI_lower),
           CI_upper = exp(CI_upper))
  exp_mean$par_name <- c("pop_max_growth_mean",
                         "pop_size_at_max_growth_mean",
                         "pop_k_mean")
  canham_sp_data <- rbind(Tree_Size_Ests$population_data[,c(1,2,4,5)],
        exp_mean)

  print(canham_sp_data)
}

sessionInfo()
