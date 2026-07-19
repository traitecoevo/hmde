#S4 methods for hmde_fit
#' hmde_fit class
#' An S4 class that inherits the class structure of a stanfit object, with the addition of slots to contain input data and prior information.
#' @slot model_level character string
#' @slot obs_data list of input data
#' @slot prior_pars list of prior parameters
#' @slot par_names list of model paramter names at each level
#' @slot model_name character string inherited from stanfit
#' @slot model_pars character vector inherited from stanfit
#' @slot par_dims list inherited from stanfit
#' @slot mode integer inherited from stanfit
#' @slot sim list inherited from stanfit
#' @slot inits list inherited from stanfit
#' @slot stan_args list inherited from stanfit
#' @slot stanmodel stanmodel object inherited from stanfit
#' @slot date character inherited from stanfit
#' @slot .MISC environment inherited from stanfit
#' @name hmde_fit-class
#' @aliases hmde_fit
#' @rdname hmde_fit-class
#' @export
# Class definition
setClass(
  Class = "hmde_fit",
  contains = "stanfit",
  representation = representation(
    model_level = "character", # Single or multi-individual
    obs_data = "list", # List of input data for model
    prior_pars = "list", # List of prior parameters for model
    par_names = "list" # List of model parameter names
  ),
  prototype = prototype(
    model_level = NA_character_,
    obs_data = list(NA),
    prior_pars = list(NA),
    par_names = list(NA)
  )
)

#' @name data_template-validator
#' @description Validation function for hmde_fit class.
#' @param object hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit
#' @keywords internal
setValidity("hmde_fit",
            function(object){
              if(length(object@model_name) != 1)
                return("'model_name' slot must be of length 1.")

              if(length(object@model_level) != 1)
                return("'model_level' slot must be of length 1.")

              TRUE
            }
)

#-----------------------------------------------------------------------------#
# Setters and getters for slots
#Model level
#' generic model_level setter
#' @param x hmde special class object
#' @rdname model_level-generic
#' @aliases model_level
#' @export
setGeneric("model_level", function(x) standardGeneric("model_level"))
#' generic model_level setter
#' @param x hmde special class object
#' @param value character string
#' @rdname model_level-generic
#' @aliases model_level<-
#' @export
setGeneric("model_level<-", function(x, value) standardGeneric("model_level<-"))

#' model_level getter
#' Getter for model_level in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("model_level", signature = "hmde_fit", function(x) x@model_level)
#' model_level setter
#' Setter for model_level in hmde_fit object
#' @param x hmde_fit class object
#' @param value character string
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("model_level<-", signature = "hmde_fit", function(x, value) {
  x@model_level <- value
  x
})

#Observation data
#' generic obs_data setter
#' @param x hmde hmde_fit
#' @rdname model_level-generic
#' @aliases model_level
#' @export
setGeneric("obs_data", function(x) standardGeneric("obs_data"))
#' generic obs_data setter
#' @param x hmde hmde_fit
#' @param value character string
#' @rdname model_level-generic
#' @aliases model_level<-
#' @export
setGeneric("obs_data<-", function(x, value) standardGeneric("obs_data<-"))

#' obs_data getter
#' Getter for obs_data in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("obs_data", signature = "hmde_fit", function(x) x@obs_data)

#' obs_data setter
#' Setter for obs_data in hmde_fit object
#' @param x hmde_fit class object
#' @param value list
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("obs_data<-", signature = "hmde_fit", function(x, value) {
  x@obs_data <- value
  x
})

#Prior paramerters
#' generic prior_pars getter
#' @param x hmde special class object
#' @rdname prior_pars-generic
#' @aliases prior_pars
#' @export
setGeneric("prior_pars", function(x) standardGeneric("prior_pars"))
#' generic prior_pars setter
#' @param x hmde special class object
#' @param value list of prior parameters
#' @rdname prior_pars-generic
#' @aliases prior_pars<-
#' @export
setGeneric("prior_pars<-", function(x, value) standardGeneric("prior_pars<-"))

#' prior_pars getter
#' Getter for prior_pars in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("prior_pars", signature = "hmde_fit", function(x) x@prior_pars)

#' prior_pars setter
#' Setter for prior_pars in hmde_fit object
#' @param x hmde_fit class object
#' @param value list
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("prior_pars<-", signature = "hmde_fit", function(x, value) {
  x@prior_pars <- value
  x
})

#Model paramerters
#' generic par_names getter
#' @param x hmde special class object
#' @rdname par_names-generic
#' @aliases par_names
#' @export
setGeneric("par_names", function(x) standardGeneric("par_names"))
#' generic par_names setter
#' @param x hmde special class object
#' @param value vector of parameter names
#' @rdname par_names-generic
#' @aliases par_names<-
#' @export
setGeneric("par_names<-", function(x, value) standardGeneric("par_names<-"))

#' par_names getter
#' Getter for par_names in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("par_names", signature = "hmde_fit", function(x) x@par_names)

#' par_names setter
#' Setter for par_names in hmde_fit object.
#' @param x hmde_fit class object
#' @param value list
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("par_names<-", signature = "hmde_fit", function(x, value) {
  x@par_names <- value
  x
})
