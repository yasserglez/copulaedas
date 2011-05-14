\name{EDA-class}

\docType{class}

\alias{EDA-class}
\alias{edaSeed,EDA-method}
\alias{edaSelect,EDA-method}
\alias{edaOptimize,EDA-method}
\alias{edaReplace,EDA-method}
\alias{edaReport,EDA-method}
\alias{edaTerminate,EDA-method}
\alias{show,EDA-method}

\title{Base Class for Estimation of Distribution Algorithms}

\description{
This is the base class of classes implementing EDAs in the package.
It is a virtual class, no objects may be created from it.
}

\section{Slots}{
\describe{
  \item{\code{name}:}{Object of class \code{"character"}.
    Descriptive name of the algorithm.}
  \item{\code{parameters}:}{Object of class \code{"list"}.
    }
}
}

\section{Methods}{
\describe{
  \item{edaSeed}{\code{signature(eda = "EDA")}: 
    The default seeding method is \code{\link{edaSeedUniform}}.}
  \item{edaSelect}{\code{signature(eda = "EDA")}:
    The default selection method is \code{\link{edaSelectTruncation}}.}
  \item{edaOptimize}{\code{signature(eda = "EDA")}:
    The default local optimization method is \code{\link{edaOptimizeDisabled}}.}
  \item{edaReplace}{\code{signature(eda = "EDA")}:
    The default replacement method is \code{\link{edaReplaceComplete}}.}
  \item{edaReport}{\code{signature(eda = "EDA")}:
    The default reporting method is \code{\link{edaReportDisabled}}.}
  \item{edaTerminate}{\code{signature(eda = "EDA")}:
    The default termination method is \code{\link{edaTerminateMaxGen}}.}
  \item{show}{\code{signature(object = "EDA")}:
    Print a text representation of the EDA.}
}
}

\seealso{
\code{\linkS4class{CEDA}},
\code{\linkS4class{VEDA}},
\code{\link{edaRun}}.
}

\keyword{classes}
