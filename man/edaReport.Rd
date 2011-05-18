\name{edaReport}

\alias{edaReport}
\alias{edaReportDisabled}
\alias{edaReportSimple}
\alias{edaReportCombined}

\title{Reporting Methods}

\description{
Method definitions for the \code{edaReport} generic function.
}

\usage{
edaReportDisabled(eda, gen, fEvals, model, pop, popEval)
edaReportSimple(eda, gen, fEvals, model, pop, popEval)
edaReportCombined(...)
}

\arguments{
  \item{eda}{An \code{\linkS4class{EDA}} instance.}

  \item{gen}{Generation.}
  
  \item{fEvals}{Number of evaluations of the objective function.}
  
  \item{model}{Model learned in the current generation.}
  
  \item{pop}{Matrix with one row for each individual in the population.}

  \item{popEval}{Vector with the evaluation of each individual in \code{pop}.}
  
  \item{\dots}{Functions implementing reporting methods.}
}

\details{
Reporting methods can be used to provide progress information during the 
evolution of the algorithm. The following reporting methods are implemented.

\describe{
  \item{\code{edaReportDisabled}}{Disable reporting progress information.}
  
  \item{\code{edaReportSimple}}{Print one line at each generation with the
    generation number, and the minimum, mean and standard deviation of the 
    evaluation of the individuals in the population.}
    
  \item{\code{edaReportCombined}}{Execute all the reporting methods specified 
    in \code{\dots} argument.}
}
}
