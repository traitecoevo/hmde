library(hexSticker)

measurement_data_transformed <- Tree_Size_Ests$measurement_data %>%
  group_by(ind_id) %>%
  mutate(
    delta_y_obs = y_obs - lag(y_obs),
    obs_interval = time - lag(time),
    obs_growth_rate = delta_y_obs/obs_interval,
    delta_y_est = y_hat - lag(y_hat),
    est_growth_rate = delta_y_est/obs_interval
  ) %>%
  ungroup()

#Plots of size over time for a sample of 5 individuals
sample_ids <- sample(1:nrow(Tree_Size_Ests$individual_data), size=5)
plot_data <- measurement_data_transformed %>%
  filter(ind_id %in% sample_ids)

plot <- ggplot(data=plot_data, aes(group = ind_id)) +
  geom_point(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
             shape = 1) +
  geom_line(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
            linetype = "dashed") +
  geom_point(aes(x = time, y=y_hat, colour = as.factor(ind_id)),
             shape = 2) +
  geom_line(aes(x = time, y=y_hat, colour = as.factor(ind_id)),
            linetype = "solid") +
  labs(x="Time (years)", y="DBH (cm)", colour="Ind. ID") +
  theme_void() +
  theme(legend.position = "none")

hexSticker::sticker(plot, package="hmde",
                    p_color = "white",
                    p_size=40,
                    p_y = 1,
                    s_x=1, s_y=1,
                    s_width=1.35, s_height = 1.35,
                    h_fill = "navy", h_color = "white",
                    filename="inst/figures/hmde_hex.png")
