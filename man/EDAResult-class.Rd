\name{EDAResult-class}

\docType{class}

\alias{EDAResult-class}
\alias{show,EDAResult-method}

\title{Class for the Results of a Run of an EDA}

\description{
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EDAResult", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{eda}:}{Object of class \code{"EDA"}.}
    \item{\code{f}:}{Object of class \code{"function"}.
      The objective function.}
    \item{\code{lower}:}{Object of class \code{"numeric"}.
      Lower bounds of the variables of the function.}
    \item{\code{upper}:}{Object of class \code{"numeric"}.
      Upper bounds of the variables of the function.}    
    \item{\code{numGens}:}{Object of class \code{"numeric"}.
      The number of generations.}
    \item{\code{fEvals}:}{Object of class \code{"numeric"}.
      The number of function evaluations.}
    \item{\code{bestEval}:}{Object of class \code{"numeric"}.
      }
    \item{\code{bestIndiv}:}{Object of class \code{"numeric"}.
      }
    \item{\code{cpuTime}:}{Object of class \code{"numeric"}.
      Run time of the algorithms (in seconds).}
  }
}
\section{Methods}{
\describe{
  \item{show}{\code{signature(object = "EDAResult")}:
    Print the results of the run of an EDA.}
}
}

\seealso{
\code{\linkS4class{EDA}},
\code{\link{edaRun}}.
}

\keyword{classes}
