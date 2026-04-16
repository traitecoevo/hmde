\name{hmde_estimates-class}
\docType{class}
\alias{hmde_estimates-class}
\alias{error_ests,hmde_estimates-method}
\alias{error_ests<-,hmde_estimates-method}
\alias{fit_summary,hmde_estimates-method}
\alias{fit_summary<-,hmde_estimates-method}
\alias{individual_ests,hmde_estimates-method}
\alias{individual_ests<-,hmde_estimates-method}
\alias{measurement_ests,hmde_estimates-method}
\alias{measurement_ests<-,hmde_estimates-method}
\alias{method,hmde_estimates-method}
\alias{method<-,hmde_estimates-method}
\alias{model_level,hmde_estimates-method}
\alias{model_level<-,hmde_estimates-method}
\alias{model_name,hmde_estimates-method}
\alias{model_name<-,hmde_estimates-method}
\alias{par_names,hmde_estimates-method}
\alias{par_names<-,hmde_estimates-method}
\alias{plot,hmde_estimates,ANY-method}
\alias{population_ests,hmde_estimates-method}
\alias{population_ests<-,hmde_estimates-method}
\alias{print,hmde_estimates-method}
\alias{prior_pars,hmde_estimates-method}
\alias{prior_pars<-,hmde_estimates-method}
\alias{runtime,hmde_estimates-method}
\alias{runtime<-,hmde_estimates-method}
\alias{show,hmde_estimates-method}
\alias{summary,hmde_estimates-method}

\title{Class \code{"hmde_estimates"}}
\description{
Object containing estimates of parameters from a hmde model, information on model fit, and prior parameters.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("hmde_estimates", ...)}.
Object has information on hmde model fit including the name of the model,
estimation method, and runtime, as well as estimates from each level of the hierarchical structure.
For single individual models, this comprises measurement and individual parameter levels,
along with error parameters. For multiple individual models there is an additional
population level. An additional slot provides the prior parameter values.
}
\section{Slots}{
  \describe{
    \item{\code{model_name}:}{Object of class \code{"character"} ~~ }
    \item{\code{model_level}:}{Object of class \code{"character"} ~~ }
    \item{\code{method}:}{Object of class \code{"character"} ~~ }
    \item{\code{runtime}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{fit_summary}:}{Object of class \code{"character"} ~~ }
    \item{\code{measurement_ests}:}{Object of class \code{"tbl_df"} ~~ }
    \item{\code{individual_ests}:}{Object of class \code{"tbl_df"} ~~ }
    \item{\code{population_ests}:}{Object of class \code{"tbl_df"} ~~ }
    \item{\code{error_ests}:}{Object of class \code{"tbl_df"} ~~ }
    \item{\code{prior_pars}:}{Object of class \code{"list"} ~~ }
    \item{\code{par_names}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{error_ests}{\code{signature(x = "hmde_estimates")}: ... }
    \item{error_ests<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{fit_summary}{\code{signature(x = "hmde_estimates")}: ... }
    \item{fit_summary<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{individual_ests}{\code{signature(x = "hmde_estimates")}: ... }
    \item{individual_ests<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{measurement_ests}{\code{signature(x = "hmde_estimates")}: ... }
    \item{measurement_ests<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{method}{\code{signature(x = "hmde_estimates")}: ... }
    \item{method<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{model_level}{\code{signature(x = "hmde_estimates")}: ... }
    \item{model_level<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{model_name}{\code{signature(x = "hmde_estimates")}: ... }
    \item{model_name<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{par_names}{\code{signature(x = "hmde_estimates")}: ... }
    \item{par_names<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{plot}{\code{signature(x = "hmde_estimates", y = "ANY")}: ... }
    \item{population_ests}{\code{signature(x = "hmde_estimates")}: ... }
    \item{population_ests<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{print}{\code{signature(x = "hmde_estimates")}: ... }
    \item{prior_pars}{\code{signature(x = "hmde_estimates")}: ... }
    \item{prior_pars<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{runtime}{\code{signature(x = "hmde_estimates")}: ... }
    \item{runtime<-}{\code{signature(x = "hmde_estimates")}: ... }
    \item{show}{\code{signature(object = "hmde_estimates")}: ... }
    \item{summary}{\code{signature(object = "hmde_estimates")}: ... }
	 }
}
\examples{
showClass("hmde_estimates")
}
\keyword{classes}
