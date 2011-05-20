\name{edaCriticalPopSize}

\alias{edaCriticalPopSize}

\title{Critical Population Size}

\description{
Determine the critical population size using a bisection method.
}

\usage{
edaCriticalPopSize(eda, f, lower, upper, fEval, fEvalTol,
                   totalRuns = 30, successRuns = totalRuns,
                   lowerPop = 2, upperPop = NA, 
                   stopPercent = 10, verbose = FALSE)
}

\arguments{
  \item{eda}{An \code{\linkS4class{EDA}} instance.}
  
  \item{f}{Objective function}
  
  \item{lower}{Lower bounds of the variables of the objective function.}
  
  \item{upper}{Upper bounds of the variables of the objective function.}
  
  \item{fEval}{Optimum value of the objective function.}
  
  \item{fEvalTol}{A run is considered successful if the difference between
    \code{fEval} and the best solution found is less than \code{fEvalTol}.}
    
  \item{totalRuns}{Total number of runs.}
  
  \item{successRuns}{Required number of successfully runs.}
  
  \item{lowerPop}{Lower bound of the initial interval for the population.}
  
  \item{upperPop}{Upper bound of the initial interval for the population.}
  
  \item{stopPercent}{Stop percent.}
  
  \item{verbose}{Print progress information.}
}

\details{
Function to determine the minimum population size required by the EDA to reach
the value \code{fEval} of the objective function in \code{successRuns} runs of a
total of \code{totalRuns} independent runs (critical population size).

The population size is determined using by a bisection method starting with the
interval delimited by the \code{lowerPop} and \code{upperPop} arguments. The
bisection procedure stops when the current population size is less than 
\code{stopPercent} percent from the critical population. If either \code{lowerPop} 
or \code{upperPop} is not specified, the algorithm will determine an initial 
interval based on the value of the \code{popSize} parameter and continue 
using the bisection method.

See (Pelikan, 2005) for the pseudocode of the algorithm in which this function is 
based.
}

\value{
Either \code{NULL} if the critical population size was not determined or
an \code{\linkS4class{EDAResults}} instance with the results of the runs of 
the EDA for the critical population size.
}

\references{
Pelikan, M. (2005). Hierarchical Bayesian Optimization Algorithm. 
Toward a new generation of Evolutionary Algorithms, Springer-Verlag.
}

\seealso{
\code{\linkS4class{EDA}},
\code{\link{edaRun}}.
}

\examples{
setMethod("edaReport", "EDA", edaReportDisabled)
setMethod("edaTerminate", "EDA", 
          edaTerminateCombined(edaTerminateEval, 
                               edaTerminateMaxEvals))

DVEDA <- VEDA(vine = "DVine", copulas = c("normal"),
              indepTestSigLevel = 0.01, fmargin = fnorm,
              pmargin = pnorm, qmargin = qnorm,
              fEval = 0, fEvalTol = 1e-03, maxEvals = 2000)
DVEDA@name <- "D-vine Estimation of Distribution Algorithm"

results <- edaCriticalPopSize(DVEDA, fSphere, rep(-600, 5), 
                              rep(600, 5), 0, 1e-03,
                              totalRuns = 10, successRuns = 10, 
                              lowerPop = 50, upperPop = 100,
                              verbose = TRUE)

show(results)
summary(results)
}
