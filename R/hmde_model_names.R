#' Returns names of available models.
#'
#' @return vector of character strings for model names.
#' @export

hmde_model_name <- function(){
  output <- c("constant_single_ind",
              "constant_multi_ind",
              "canham_single_ind",
              "canham_multi_ind",
              "vb_single_ind",
              "vb_multi_ind",
              "linear_single_ind")

  return(output)
}
