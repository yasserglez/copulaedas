\name{edaIndepRuns}

\alias{edaIndepRuns}

\title{Independent Runs}

\description{
Run a sequence of independent runs.
}

\usage{
edaIndepRuns(eda, f, lower, upper, runs, verbose = FALSE)
}

\arguments{
\item{eda}{
  An \code{\linkS4class{EDA}} instance.}
\item{f}{
  The objective function.}
\item{lower}{
  A \code{"numeric"} vector with the lower bounds of the variables of the function.}
\item{upper}{
  A \code{"numeric"} vector with the upper bounds of the variables of the function.}
\item{runs}{
  The number of runs.}
\item{verbose}{
  Print information after each run and a final summary.}
}

\value{
An \code{\linkS4class{EDAResults}} instance.
}

\seealso{
\code{\linkS4class{EDA}},
\code{\link{edaRun}}.
}

\examples{
setMethod("edaTerminate", "EDA", edaTerminateEval)

DVEDA <- VEDA(vine = "DVine", 
              copulas = c("normal", "clayton", "gumbel"),
              indepTestSigLevel = 0.01, fmargin = fnorm,
              pmargin = pnorm, qmargin = qnorm, 
              popSize = 200, fEval = 0, fEvalTol = 1e-03)
DVEDA@name <- "D-vine Estimation of Distribution Algorithm"

results <- edaIndepRuns(DVEDA, fSphere, rep(-600, 5), rep(600, 5), 3)

show(results)
summary(results)
}
