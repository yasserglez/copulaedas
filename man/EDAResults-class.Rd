\name{EDAResults-class}

\docType{class}

\alias{EDAResults-class}
\alias{show,EDAResults-method}
\alias{summary,EDAResults-method}

\title{Class for the Results of a Sequence of Runs of an EDA}

\description{
Results of a sequence of independent runs of an EDA. This class is just a 
wrapper for a \code{list} object with \code{\linkS4class{EDAResult}} 
instances.
}

\section{Objects from the Class}{
Objects are created by calling the \code{\link{edaIndepRuns}} function.
}

\section{Methods}{
\describe{
  \item{show}{\code{signature(object = "EDAResults")}: Print a table with 
    the results.}

  \item{summary}{\code{signature(object = "EDAResults")}: Print a summary 
    of the results.}
}
}

\seealso{
\code{\linkS4class{EDA}},
\code{\link{edaIndepRuns}}.
}

\keyword{classes}
