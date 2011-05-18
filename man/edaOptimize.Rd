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
  \item{eda}{An \code{\linkS4class{EDA}} instance.}

  \item{gen}{Generation.}

  \item{pop}{Matrix with one row for each individual in the population.}

  \item{popEval}{Vector with the evaluation of each individual in \code{pop}.}

  \item{f}{Objective function}

  \item{lower}{Lower bounds of the variables of the objective function.}
  
  \item{upper}{Upper bounds of the variables of the objective function.}
}

\details{
Local optimization methods are used to improve the solutions sampled by the 
search distribution. The following local optimization methods are implemented.

\describe{
  \item{\code{edaOptimizeDisabled}}{Disable the local optimization.}
}
}

\value{
A \code{"list"} object with the following components.

  \item{pop}{Matrix with one row for each individual in the optimized population.}
  
  \item{popEval}{Vector with the evaluation of each individual in the optimized 
    population.}
}
