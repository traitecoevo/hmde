\name{hmde_data_template-class}
\docType{class}
\alias{hmde_data_template-class}
\alias{model_level,hmde_data_template-method}
\alias{model_level<-,hmde_data_template-method}
\alias{model_name,hmde_data_template-method}
\alias{model_name<-,hmde_data_template-method}
\alias{obs_data,hmde_data_template-method}
\alias{obs_data<-,hmde_data_template-method}
\alias{par_names,hmde_data_template-method}
\alias{par_names<-,hmde_data_template-method}
\alias{plot,hmde_data_template,ANY-method}
\alias{print,hmde_data_template-method}
\alias{prior_pars,hmde_data_template-method}
\alias{prior_pars<-,hmde_data_template-method}
\alias{show,hmde_data_template-method}
\alias{summary,hmde_data_template-method}

\title{Class \code{"hmde_data_template"}}
\description{
Data structure containing information about a hmde model to be fit,
the data it is fit to, and prior specification. Acts as input for
the \code{"hmde_run"} function.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("hmde_data_template", ...)}.
Objects contain information about the intended model to be fit, and the data
that it will be fit to.
}
\section{Slots}{
  \describe{
    \item{\code{model_name}:}{Object of class \code{"character"} ~~ }
    \item{\code{model_level}:}{Object of class \code{"character"} ~~ }
    \item{\code{obs_data}:}{Object of class \code{"list"} ~~ }
    \item{\code{prior_pars}:}{Object of class \code{"list"} ~~ }
    \item{\code{par_names}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{model_level}{\code{signature(x = "hmde_data_template")}: ... }
    \item{model_level<-}{\code{signature(x = "hmde_data_template")}: ... }
    \item{model_name}{\code{signature(x = "hmde_data_template")}: ... }
    \item{model_name<-}{\code{signature(x = "hmde_data_template")}: ... }
    \item{obs_data}{\code{signature(x = "hmde_data_template")}: ... }
    \item{obs_data<-}{\code{signature(x = "hmde_data_template")}: ... }
    \item{par_names}{\code{signature(x = "hmde_data_template")}: ... }
    \item{par_names<-}{\code{signature(x = "hmde_data_template")}: ... }
    \item{plot}{\code{signature(x = "hmde_data_template", y = "ANY")}: ... }
    \item{print}{\code{signature(x = "hmde_data_template")}: ... }
    \item{prior_pars}{\code{signature(x = "hmde_data_template")}: ... }
    \item{prior_pars<-}{\code{signature(x = "hmde_data_template")}: ... }
    \item{show}{\code{signature(object = "hmde_data_template")}: ... }
    \item{summary}{\code{signature(object = "hmde_data_template")}: ... }
	 }
}

\examples{
showClass("hmde_data_template")
}
\keyword{classes}
