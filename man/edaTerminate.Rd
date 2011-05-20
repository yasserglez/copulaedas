\name{edaTerminate}

\alias{edaTerminate}
\alias{edaTerminateMaxGen}
\alias{edaTerminateMaxEvals}
\alias{edaTerminateEval}
\alias{edaTerminateEvalStdDev}
\alias{edaTerminateCombined}

\title{Termination Methods}

\description{
Method definitions for the \code{edaTerminate} generic function.
}

\usage{
edaTerminateMaxGen(eda, gen, fEvals, pop, popEval)
edaTerminateMaxEvals(eda, gen, fEvals, pop, popEval)
edaTerminateEval(eda, gen, fEvals, pop, popEval)
edaTerminateEvalStdDev(eda, gen, fEvals, pop, popEval)
edaTerminateCombined(...)
}

\arguments{
  \item{eda}{An \code{\linkS4class{EDA}} instance.}

  \item{gen}{Generation.}
  
  \item{fEvals}{Number of evaluations of the objective function.}  

  \item{pop}{Matrix with one row for each individual in the population.}

  \item{popEval}{Vector with the evaluation of each individual in \code{pop}.}
  
  \item{\dots}{Functions implementing termination methods.}
}

\details{
Termination methods decide when to stop the main loop of the EDA. 
The following termination methods are implemented.

\describe{
  \item{\code{edaTerminateMaxGen}}{Stop when a maximum number of generations 
    has been reached. The parameter \code{maxGen} specifies the number of 
    generations (default value: \code{100}).}

  \item{\code{edaTerminateMaxEvals}}{Stop when a maximum number of evaluations 
    has been reached. The parameter \code{maxEvals} specifies the number of 
    generations (default value: \code{1000}.)}

  \item{\code{edaTerminateEval}}{Stop when a given value of the objective 
    function has been reached. The parameters \code{fEval} (default value: 
    \code{0}) and \code{fEvalTol} (default value: \code{1e-06}) set the value 
    of the objective function and the tolerance, respectively.}

  \item{\code{edaTerminateEvalStdDev}}{Stop when the standard deviation of 
    the evaluation of the individuals in the population is less than a value 
    given by the parameter \code{fEvalStdDev} (default value: \code{1e-02})}.

  \item{\code{edaTerminateCombined}}{Evaluate all the termination criteria 
    specified in \code{\dots} and stop if (at least) one of them returns 
    \code{TRUE}.}
}
}

\value{
A \code{"logical"} value indicating if the algorithm should terminate.
}
