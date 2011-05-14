\name{CEDA-class}

\docType{class}

\alias{CEDA}
\alias{CEDA-class}
\alias{edaLearnCEDA}
\alias{edaSampleCEDA}
\alias{edaLearn,CEDA-method}
\alias{edaSample,CEDA-method}

\title{Class for Copula Estimation of Distribution Algorithms}

\description{
Extends the \code{\linkS4class{EDA}} class to implement Copula Estimation of 
Distribution Algorithms.
}

\section{Objects from the Class}{
Objects can be created by calls to the \code{CEDA} function as illustrated
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
  \item{edaLearn}{\code{signature(eda = "CEDA")}:
    The \code{edaLearnCEDA} function.}
  \item{edaSample}{\code{signature(eda = "CEDA")}:
    The \code{edaSampleCEDA} function.}
}
}

\references{
Arderí, R. J. (2007). Algoritmo con Estimación de Distribuciones con Cópula
Gaussiana, Diploma Thesis, University of Havana.

Soto, M. and Ochoa, A. and Arderí, R. J. (2007). El Algoritmo con Estimación 
de Distribuciones basado en Cópula Gausiana, Technical Report ICIMAF 2007-406,
Institute of Cybernetics, Mathematics, and Physics.
}

\examples{
}

\keyword{classes}
