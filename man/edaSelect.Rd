\name{edaSelect}

\alias{edaSelect}
\alias{edaSelectTruncation}

\title{Selection Methods}

\description{
Methods for the \code{edaSelect} generic function.
}

\usage{
edaSelectTruncation(eda, gen, pop, popEval)
}

\arguments{
  \item{eda}{\code{\linkS4class{EDA}} instance.}

  \item{gen}{Generation.}

  \item{pop}{Matrix with one row for each individual in the population.}

  \item{popEval}{Vector with the evaluation of each individual in \code{pop}.}
}

\details{
Selection methods determine the individuals to be modeled by the search 
distribution. These individuals are usually the most promising individuals 
of the population. The following selection methods are implemented.

\describe{
  \item{\code{edaSelectTruncation}}{Select the best \code{100*truncFactor} 
    percent of individuals with the best fitness in the population. The 
    parameter \code{truncFactor} specifies the truncation factor (default 
    value: \code{0.3}).}
}
}

\value{
An \code{integer} vector with the indexes of the individuals selected 
from \code{pop}.
}
