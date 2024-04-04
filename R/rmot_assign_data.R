#' Assign data to template
#'
#' @param model_template output from rmot_model
#' @param ... data-masking name-value pairs
#'
#' @return updated named list with your data assigned to Stan model parameters
#' @export
#'
#' @examples
#' rmot_model("linear") |> rmot_assign_data(X = Loblolly$age, Y = Loblolly$height)
rmot_assign_data <- function(model_template, ...){
  # Grab user expressions
  user_code <- rlang::enquos(..., .check_assign = TRUE)

  # Grab the names
  fields <- names(user_code)

  #TODO: Check names are in model_template

  # Evaluate the RHS of expressions (the values)
  data <- purrr::map(user_code,
                     ~rlang::eval_tidy(.x, env = rlang::caller_env())
                     )

  for(i in fields){
    model_template <- purrr::list_modify(model_template, !!!data[i])
  }

  #TODO: Check if N is supplied, if not, give error

  return(model_template)
}
