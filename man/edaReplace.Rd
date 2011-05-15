\name{edaReplace}

\alias{edaReplace}
\alias{edaReplaceComplete}

\title{Replacement Methods}

\description{
Method definitions for the \code{edaReplace} generic function.
}

\usage{
edaReplaceComplete(eda, gen, pop, popEval, sampledPop, sampledEval)
}

\arguments{
\item{eda}{
  An \code{\linkS4class{EDA}} instance.}
\item{gen}{
  Generation.}
\item{pop}{
  A \code{"matrix"} with one row for each individual in the population
  of the previous generation.}
\item{popEval}{
  A \code{"numeric"} vector with the evaluation of the individuals in \code{pop}.}
\item{sampledPop}{
  A \code{"matrix"} with one row for each individual sampled in the current 
  generation. The local optimization method has been already applied.}
\item{sampledEval}{
  A \code{"numeric"} vector with the evaluation of the individuals in 
  \code{sampledPop}.}
}

\details{
Replacement methods are used to combine the individuals sampled in the
current generation with individuals from the population of the previous
generation. The following replacement methods are implemented.

\describe{
\item{\code{edaReplaceComplete}}{
  The population sampled in the current generation completely replaces
  the population of the previous generation.}
}
}

\value{
A \code{"list"} object with the following components.

\item{pop}{
  A \code{"matrix"} with one row for each individual in the resulting population.}
\item{popEval}{
  A \code{"numeric"} vector with the evaluation of each individual in the
  resulting population.}
}
