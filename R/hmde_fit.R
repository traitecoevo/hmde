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

              if(!inherits(object, "stanfit"))
                return("object not stanfit class.")

              TRUE
            }
)

#-----------------------------------------------------------------------------#
# Setters and getters for slots
#Model level
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


#model_name
#' model_name getter
#' Getter for model_name in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("model_name", signature = "hmde_fit", function(x) x@model_name)
#' model_name setter
#' Setter for model_name in hmde_fit object
#' @param x hmde_fit class object
#' @param value character string
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("model_name<-", signature = "hmde_fit", function(x, value) {
  x@model_name <- value
  x
})

#model_pars
#' generic model_pars setter
#' @param x hmde special class object
#' @rdname model_pars-generic
#' @aliases model_pars
#' @export
setGeneric("model_pars", function(x) standardGeneric("model_pars"))
#' generic model_pars setter
#' @param x hmde special class object
#' @param value character vector
#' @rdname model_pars-generic
#' @aliases model_pars<-
#' @export
setGeneric("model_pars<-", function(x, value) standardGeneric("model_pars<-"))

#' model_pars getter
#' Getter for model_pars in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("model_pars", signature = "hmde_fit", function(x) x@model_pars)
#' model_pars setter
#' Setter for model_pars in hmde_fit object
#' @param x hmde_fit class object
#' @param value character vector
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("model_pars<-", signature = "hmde_fit", function(x, value) {
  x@model_pars <- value
  x
})

#par_dims
#' generic par_dims setter
#' @param x hmde special class object
#' @rdname par_dims-generic
#' @aliases par_dims
#' @export
setGeneric("par_dims", function(x) standardGeneric("par_dims"))
#' generic par_dims setter
#' @param x hmde special class object
#' @param value list
#' @rdname par_dims-generic
#' @aliases par_dims<-
#' @export
setGeneric("par_dims<-", function(x, value) standardGeneric("par_dims<-"))

#' par_dims getter
#' Getter for par_dims in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("par_dims", signature = "hmde_fit", function(x) x@par_dims)
#' par_dims setter
#' Setter for par_dims in hmde_fit object
#' @param x hmde_fit class object
#' @param value list
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("par_dims<-", signature = "hmde_fit", function(x, value) {
  x@par_dims <- value
  x
})

#mode
#' generic mode setter
#' @param x hmde special class object
#' @rdname mode-generic
#' @aliases mode
#' @export
setGeneric("mode", function(x) standardGeneric("mode"))
#' generic mode setter
#' @param x hmde special class object
#' @param value integer vector
#' @rdname mode-generic
#' @aliases mode<-
#' @export
setGeneric("mode<-", function(x, value) standardGeneric("mode<-"))

#' mode getter
#' Getter for mode in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("mode", signature = "hmde_fit", function(x) x@mode)
#' mode setter
#' Setter for mode in hmde_fit object
#' @param x hmde_fit class object
#' @param value integer vector
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("mode<-", signature = "hmde_fit", function(x, value) {
  x@mode <- value
  x
})

#sim
#' generic sim setter
#' @param x hmde special class object
#' @rdname sim-generic
#' @aliases sim
#' @export
setGeneric("sim", function(x) standardGeneric("sim"))
#' generic sim setter
#' @param x hmde special class object
#' @param value list
#' @rdname sim-generic
#' @aliases sim<-
#' @export
setGeneric("sim<-", function(x, value) standardGeneric("sim<-"))

#' sim getter
#' Getter for sim in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("sim", signature = "hmde_fit", function(x) x@sim)
#' sim setter
#' Setter for sim in hmde_fit object
#' @param x hmde_fit class object
#' @param value list
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("sim<-", signature = "hmde_fit", function(x, value) {
  x@sim <- value
  x
})

#inits
#' generic inits setter
#' @param x hmde special class object
#' @rdname inits-generic
#' @aliases inits
#' @export
setGeneric("inits", function(x) standardGeneric("inits"))
#' generic inits setter
#' @param x hmde special class object
#' @param value list
#' @rdname inits-generic
#' @aliases inits<-
#' @export
setGeneric("inits<-", function(x, value) standardGeneric("inits<-"))

#' inits getter
#' Getter for inits in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("inits", signature = "hmde_fit", function(x) x@inits)
#' inits setter
#' Setter for inits in hmde_fit object
#' @param x hmde_fit class object
#' @param value list
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("inits<-", signature = "hmde_fit", function(x, value) {
  x@inits <- value
  x
})

#stan_args
#' generic stan_args setter
#' @param x hmde special class object
#' @rdname stan_args-generic
#' @aliases stan_args
#' @export
setGeneric("stan_args", function(x) standardGeneric("stan_args"))
#' generic stan_args setter
#' @param x hmde special class object
#' @param value list
#' @rdname stan_args-generic
#' @aliases stan_args<-
#' @export
setGeneric("stan_args<-", function(x, value) standardGeneric("stan_args<-"))

#' stan_args getter
#' Getter for stan_args in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("stan_args", signature = "hmde_fit", function(x) x@stan_args)
#' stan_args setter
#' Setter for stan_args in hmde_fit object
#' @param x hmde_fit class object
#' @param value list
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("stan_args<-", signature = "hmde_fit", function(x, value) {
  x@stan_args <- value
  x
})

#stanmodel
#' generic stanmodel setter
#' @param x hmde special class object
#' @rdname stanmodel-generic
#' @aliases stanmodel
#' @export
setGeneric("stanmodel", function(x) standardGeneric("stanmodel"))
#' generic stanmodel setter
#' @param x hmde special class object
#' @param value stanmodel object
#' @rdname stanmodel-generic
#' @aliases stanmodel<-
#' @export
setGeneric("stanmodel<-", function(x, value) standardGeneric("stanmodel<-"))

#' stanmodel getter
#' Getter for stanmodel in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("stanmodel", signature = "hmde_fit", function(x) x@stanmodel)
#' stanmodel setter
#' Setter for stanmodel in hmde_fit object
#' @param x hmde_fit class object
#' @param value stanmodel object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod("stanmodel<-", signature = "hmde_fit", function(x, value) {
  x@stanmodel <- value
  x
})

#.MISC
#' generic .MISC setter
#' @param x hmde special class object
#' @rdname MISC-generic
#' @aliases .MISC
#' @export
setGeneric(".MISC", function(x) standardGeneric(".MISC"))
#' generic .MISC setter
#' @param x hmde special class object
#' @param value environment
#' @rdname MISC-generic
#' @aliases .MISC<-
#' @export
setGeneric(".MISC<-", function(x, value) standardGeneric(".MISC<-"))

#' .MISC getter
#' Getter for .MISC in hmde_fit object
#' @param x hmde_fit class object
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod(".MISC", signature = "hmde_fit", function(x) x@.MISC)
#' .MISC setter
#' Setter for .MISC in hmde_fit object
#' @param x hmde_fit class object
#' @param value environment
#' @rdname hmde_fit-class
#' @aliases hmde_fit-class
#' @export
setMethod(".MISC<-", signature = "hmde_fit", function(x, value) {
  x@.MISC <- value
  x
})


#------------------------------------------------------------------------------#
# Helper functions

# Constructor
#' Constructor function for hmde_fit class, internal function for hmde_run_model.
#'
#' @param data_template hmde_data_template class object
#' @param fit stanfit class object
#' @return hmde_fit class object
#'
#' @rdname hmde_data_template-class
#' @aliases hmde_data_template-class
#' @keywords internal
hmde_fit <- function(data_template, #Mandatory hmde_data_template object
                     fit #Mandatory stanfit object
                     ){
  #Validation
  if(!inherits(data_template, "hmde_data_template")){
    stop("data_template not of hmde_data_template class.")
  }

  if(!inherits(fit, "stanfit")){
    stop("fit not of stanfit class.")
  }

  if(!model_name(data_template) %in% hmde_model_names()){
    stop("Model name not recognised. Run hmde_model_names() to see available models.")
  }

  if(model_name(data_template) != fit@model_name){
    stop("Model name for data_template and fit different.")
  }

  #Initial object
  out <- new("hmde_fit")

  #Assign slots
  model_level(out) <- model_level(data_template)
  obs_data(out) <- obs_data(data_template)
  prior_pars(out) <- prior_pars(data_template)
  par_names(out) <- par_names(data_template)
  model_name(out) <- fit@model_name
  model_pars(out) <- fit@model_pars
  par_dims(out) <- fit@par_dims
  mode(out) <- fit@mode
  sim(out) <- fit@sim
  inits(out) <- fit@inits
  stan_args(out) <- fit@stan_args
  stanmodel(out) <- fit@stanmodel
  date(out) <- fit@date
  .MISC(out) <- fit@.MISC

  #Validate
  #Check validity
  if(!validObject(out)){
    stop("Invalid hmde_fit object.")
  }

  return(out)
}
