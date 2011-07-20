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
Base class of all the classes implementing EDAs in the package.
}

\section{Slots}{
\describe{
  \item{\code{name}:}{Object of class \code{"character"}. Name of the EDA.}
  
  \item{\code{parameters}:}{Object of class \code{"list"}. Parameters of the EDA.}
}
}

\section{Methods}{
\describe{
  \item{edaSeed}{\code{signature(eda = "EDA")}: Seeding method. 
    Default method: \code{\link{edaSeedUniform}}.}
    
  \item{edaSelect}{\code{signature(eda = "EDA")}: Selection method. 
    Default method: \code{\link{edaSelectTruncation}}.}
    
  \item{edaOptimize}{\code{signature(eda = "EDA")}: Local optimization method. 
    Default method: \code{\link{edaOptimizeDisabled}}.}
    
  \item{edaReplace}{\code{signature(eda = "EDA")}: Replacement method.
    Default method: \code{\link{edaReplaceComplete}}.}
    
  \item{edaReport}{\code{signature(eda = "EDA")}: Reporting method.
    Default method: \code{\link{edaReportDisabled}}.}
    
  \item{edaTerminate}{\code{signature(eda = "EDA")}: Termination method. 
    Default method: \code{\link{edaTerminateMaxGen}}.}
    
  \item{show}{\code{signature(object = "EDA")}: Print a textual representation 
    of the EDA.}
}
}

\seealso{
\code{\linkS4class{CEDA}},
\code{\linkS4class{VEDA}},
\code{\link{edaRun}}.
}

\keyword{classes}
