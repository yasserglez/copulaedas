\name{VEDA-class}

\docType{class}

\alias{VEDA}
\alias{VEDA-class}
\alias{edaLearnVEDA}
\alias{edaSampleVEDA}
\alias{edaLearn,VEDA-method}
\alias{edaSample,VEDA-method}

\title{Class for Vine Estimation of Distribution Algorithms}

\description{
Extends the \code{\linkS4class{EDA}} class to implement Vine Estimation of
Distribution Algorithms.
}

\section{Objects from the Class}{
Objects can be created by calls to the \code{VEDA} function as illustrated
in the examples.
}

\section{Slots}{
\describe{
  \item{\code{name}:}{See the documentation of the slot in the
  	\code{\linkS4class{EDA}} class.}
  \item{\code{parameters}:}{See the documentation of the slot in the
  	\code{\linkS4class{EDA}} class.}
}
}

\section{Methods}{
\describe{
  \item{edaLearn}{\code{signature(eda = "VEDA")}:
    The \code{edaLearnVEDA} function.}
  \item{edaSample}{\code{signature(eda = "VEDA")}:
    The \code{edaSampleVEDA} function.}
}
}

\references{
Soto, M. and González-Fernández, Y. (2010). Vine Estimation of Distribution
Algorithms, Technical Report ICIMAF 2010-561, Institute of Cybernetics,
Mathematics, and Physics.
}

\examples{
}

\keyword{classes}
