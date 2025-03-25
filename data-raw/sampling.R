# Code to produce samples for demonstration data.
# Original data files are not included due to size, but links to the sources
# are available.

#-----------------------------------------------------------------------------#
#Libraries
library(tidyverse)
library(devtools)

#-----------------------------------------------------------------------------#
#Trout size data - Constant model
#Source: Moe et al 2020 https://doi.org/10.3897/BDJ.8.e52157
HunderTrout <- read.csv("data-raw/HunderTroutData_CMRR.csv")

hist(HunderTrout$CaptureNoMax)

FilteredHunderTrout <- HunderTrout %>%
  na.omit() %>%
  group_by(MarkNo) %>%
  mutate(MaxObs = n()) %>%
  ungroup()

#Stratified SWSRWR, 25x 2 obs, 15x 3 obs, 10x 4obs, transform to get time between observations
obs <- c(2, 3, 4)
sample_size <- c(25, 15, 10)
trout_data <- tibble()
for(i in 1:length(obs)){
  temp <- FilteredHunderTrout %>%
    filter(MaxObs == obs[i]) %>%
    select(MarkNo) %>%
    distinct()
  sample_ids <- sample(temp$MarkNo, size = sample_size[i])

  sample_data <- FilteredHunderTrout %>%
    filter(MarkNo %in% sample_ids) %>%
    arrange(MarkNo, CaptureNo)

  trout_data <- rbind(trout_data, sample_data)
}

Trout_Size_Data <- trout_data %>%
  mutate(y_obs = LengthAtCapture,
         date = as.Date(CaptureDate, format="%Y-%m-%d"),
         ind_id = as.numeric(as.factor(MarkNo))) %>%
  arrange(ind_id, date) %>%
  group_by(ind_id) %>%
  mutate(time = as.numeric(difftime(date, first(date), units="days")),
         time = time/365.25,
         obs_index = rank(time)) %>%
  ungroup() %>%
  select(ind_id, time, y_obs, obs_index)


usethis::use_data(Trout_Size_Data)

#-----------------------------------------------------------------------------#
#Lizard size data - VB model
#Source: Kar, Nakagawa, & Noble 2024  https://doi.org/10.17605/OSF.IO/HJKXD
# SVL - tip of snout to beginning of cloaca opening

lizards <- read.csv("data-raw/Ld_growth_bd_date_cleaned.csv")

transformed_lizd_data <- lizards %>%
  na.omit() %>%
  filter(dup_check == "Original") %>%
  mutate(ind_id = as.factor(bd_liz_id)) %>%
  group_by(ind_id) %>%
  summarise(MaxObs = n()) %>%
  ungroup() %>%
  select(ind_id, MaxObs) %>%
  distinct() %>%
  filter(MaxObs >=5)

hist(transformed_lizd_data$MaxObs)

#Sample of 50, transform to get time between weight values
sample_ids <- sample(transformed_lizd_data$ind_id, size=50)
Lizard_Size_Data <- lizards %>%
  na.omit() %>%
  filter(bd_liz_id %in% sample_ids,
         dup_check == "Original") %>%
  mutate(y_obs = bd_svl,
         ind_id = as.numeric(as.factor(bd_liz_id)),
         bd_date = str_replace(bd_date, "/", "-"),
         bd_date = str_replace(bd_date, "/", "-"),
         date = as.Date(bd_date, format="%m-%d-%Y")) %>%
  group_by(ind_id) %>%
  mutate(time = as.numeric(difftime(date, first(date), units="days")),
         obs_index = rank(time)) %>%
  ungroup() %>%
  select(ind_id, time, y_obs, obs_index)

usethis::use_data(Lizard_Size_Data)


#-----------------------------------------------------------------------------#

#Tree size data - Canham model
#Data source: Condit et al 2019  https://doi.org/10.15146/5xcp-0d46
#The G. recondita data the sample is taken from has undergone pre-processing
grecondita <- readRDS("data-raw/gar2in_rstan_data.rds")
tree_raw_data <- tibble(
  y_obs = grecondita$S_obs,
  census_interval = c(0,grecondita$census_interval[1:length(grecondita$census_interval)-1]),
  obs_index = grecondita$census,
  tree_id = grecondita$treeid_factor
) %>%
  group_by(tree_id) %>%
  mutate(obs_growth = last(y_obs) - first(y_obs)) %>%
  ungroup() %>%
  filter(obs_growth > 3)

sample_ids <- sample(unique(tree_raw_data$tree_id), size=50)

Tree_Size_Data <- tree_raw_data %>%
  filter(as.numeric(tree_id) %in% sample_ids) %>%
  mutate(ind_id = as.numeric(as.factor(tree_id))) %>%
  arrange(ind_id, obs_index) %>%
  group_by(ind_id) %>%
  mutate(time = cumsum(census_interval)) %>%
  ungroup() %>%
  select(ind_id, time, y_obs, obs_index)

usethis::use_data(Tree_Size_Data, overwrite = TRUE)

#After fitting the model, save the estimates.
usethis::use_data(Tree_Size_Ests, overwrite = TRUE)
