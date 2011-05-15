\name{edaOptimize}

\alias{edaOptimize}
\alias{edaOptimizeDisabled}

\title{Local Optimization Methods}

\description{
Method definitions for the \code{edaOptimize} generic function.
}

\usage{
edaOptimizeDisabled(eda, gen, pop, popEval, f, lower, upper)
}

\arguments{
\item{eda}{
  An \code{\linkS4class{EDA}} instance.}
\item{gen}{
  Generation.}
\item{pop}{
  A \code{"matrix"} with one row for each individual in the population.}
\item{popEval}{
  A \code{"numeric"} vector with the evaluation of each individual in \code{pop}.}
\item{f}{
  The objective function.}
\item{lower}{
  A \code{"numeric"} vector with the lower bounds of the variables of the function.}
\item{upper}{
  A \code{"numeric"} vector with the upper bounds of the variables of the function.}
}

\details{
Local optimization methods are used to improve the solutions sampled by the 
search distribution. The following local optimization methods are implemented.

\describe{
\item{\code{edaOptimizeDisabled}}{
  Disable the local optimization.}
}
}

\value{
A \code{"list"} object with the following components.

\item{pop}{
  A \code{"matrix"} with one row for each individual in the optimized population.}  
\item{popEval}{
  A \code{"numeric"} vector with the evaluation of each individual in the
  optimized population.}
}
