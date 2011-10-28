\name{edaReplace}

\alias{edaReplace}
\alias{edaReplaceComplete}

\title{Replacement Methods}

\description{
Methods for the \code{edaReplace} generic function.
}

\usage{
edaReplaceComplete(eda, gen, pop, popEval, sampledPop, sampledEval)
}

\arguments{
  \item{eda}{\code{\linkS4class{EDA}} instance.}

  \item{gen}{Generation.}

  \item{pop}{Matrix with one row for each individual in the population.}

  \item{popEval}{Vector with the evaluation of each individual in \code{pop}.}

  \item{sampledPop}{Matrix with one row for each individual sampled in the
    current generation.}

  \item{sampledEval}{Vector with the evaluation of the individuals in
    \code{sampledPop}.}
}

\details{
Replacement methods combine the individuals sampled in the current generation 
with individuals from the population of the previous generation. The following 
replacement methods are implemented.

\describe{
  \item{\code{edaReplaceComplete}}{The population sampled in the current 
    generation completely replaces the population of the previous generation.}
}
}

\value{
A \code{list} with the following components.

\item{pop}{Matrix with one row for each individual in the resulting 
  population.}

\item{popEval}{Vector with the evaluation of each individual in the 
  resulting population.}
}
