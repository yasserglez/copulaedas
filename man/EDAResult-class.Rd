\name{EDAResult-class}

\docType{class}

\alias{EDAResult-class}
\alias{show,EDAResult-method}

\title{Class for the Results of a Run of an EDA}

\description{
Results of a run of an EDA.
}

\section{Objects from the Class}{
Objects are created by calling the \code{\link{edaRun}} function.
}

\section{Slots}{
\describe{
  \item{\code{eda}:}{Object of class \code{"EDA"}.}
  \item{\code{f}:}{Object of class \code{"function"}. Objective function.}
  \item{\code{lower}:}{Object of class \code{"numeric"}. Lower bounds of the 
    variables of the objective function.}
  \item{\code{upper}:}{Object of class \code{"numeric"}. Upper bounds of the 
    variables of the objective function.}    
  \item{\code{numGens}:}{Object of class \code{"numeric"}. Number of generations.}
  \item{\code{fEvals}:}{Object of class \code{"numeric"}. Number of evaluations 
    of the objective function.}
  \item{\code{bestEval}:}{Object of class \code{"numeric"}. Best evaluation of 
    the objective function.}
  \item{\code{bestIndiv}:}{Object of class \code{"numeric"}. Best individual.}
  \item{\code{cpuTime}:}{Object of class \code{"numeric"}. Run time of the 
    algorithms in seconds.}
}
}
\section{Methods}{
\describe{
  \item{show}{\code{signature(object = "EDAResult")}: Print the results of the run.}
}
}

\seealso{
\code{\linkS4class{EDA}},
\code{\link{edaRun}}.
}

\keyword{classes}
