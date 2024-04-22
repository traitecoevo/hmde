#' SUSTAIN Salmo trutta data
#'
#' A subset of data from the SUSTAIN trout capture-recapture data set
#' from Moe et al. (2020).
#' Data prepared by taking a stratified sample of individual IDs based on the
#' number of observations per individual: 25 individuals with 2 observations,
#' 15 with 3, 10 with 4. Within the groups a simple random sample
#' without replacement was used. Data was then transformed and
#' renamed to match the required structure to act as demonstration for
#' the package.
#'
#' @format ## `Trout_Size_Data`
#' A data frame with 135 rows and 4 columns:
#' \describe{
#'   \item{ind_id}{ID number for individual}
#'   \item{time}{Years since first capture and tagging of individual.}
#'   \item{y_obs}{Individual length in centimetres.}
#'   \item{obs_index}{Index of observations for individual}
#' }
#' @source <https://doi.org/10.3897/BDJ.8.e52157>
"Trout_Size_Data"

#' Garcinia recondita - Barro Colorado Island data
#'
#' A subset of data from the Barro Colorado Island long term forest plot
#' managed by the Smithsonian Tropical Research Institute (Condit et al. 2019).
#' Data was prepared by taking a simple random sample without replacement of
#' 50 individual IDs from Garcinia recondita. The sampling frame was restricted
#' to individuals with 6 observations since 1990, and a difference between
#' observed max and min sizes of more than 1.5cm in order to avoid identifiability
#' issues. Data was then transformed and renamed to match  the required structure
#' to act as demonstration for the package.
#'
#' @format ## `Tree_Size_Data`
#' A data frame with 300 rows and 4 columns:
#' \describe{
#'   \item{ind_id}{ID number for individual}
#'   \item{time}{Years since first observation.}
#'   \item{y_obs}{Individual diameter at breast height (DBH) in centimetres.}
#'   \item{obs_index}{Index of observations for individual}
#' }
#' @references \url{https://doi.org/10.1002/ecy.4140}
#' @source \url{https://doi.org/10.15146/5xcp-0d46}
"Tree_Size_Data"

#' Skink size data - Lampropholis delicata
#'
#' A subset of data from Kar, Nakagawa, and Noble (2024), used to model growth
#' behaviour in a skink species.
#' Data was prepared by taking a simple random sample with replacement of 50
#' individual IDs among individuals with at least 5 observations each. Data was
#' then transformed to conform to the needs of a model data set in the package.
#'
#' @format ## `Lizard_Mass_Data`
#' A data frame with 336 rows and 4 columns:
#' \describe{
#'   \item{ind_id}{ID number for individual}
#'   \item{time}{Days since first observation.}
#'   \item{y_obs}{Individual mass in kilograms.}
#'   \item{obs_index}{Index of observations for individual}
#' }
#'
#' @source \url{https://osf.io/hjkxd/}
"Lizard_Mass_Data"
