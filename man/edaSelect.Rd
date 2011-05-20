\name{edaSelect}

\alias{edaSelect}
\alias{edaSelectTruncation}

\title{Selection Methods}

\description{
Method definitions for the \code{edaSelect} generic function.
}

\usage{
edaSelectTruncation(eda, gen, pop, popEval)
}

\arguments{
  \item{eda}{An \code{\linkS4class{EDA}} instance.}

  \item{gen}{Generation.}

  \item{pop}{Matrix with one row for each individual in the population.}

  \item{popEval}{Vector with the evaluation of each individual in \code{pop}.}
}

\details{
Selection methods determine the individuals that will be modeled by the search 
distribution estimated using the \code{edaLearn} function. Usually these individuals 
are the most promising individuals of the population. The following selection 
methods are implemented.

\describe{
  \item{\code{edaSelectTruncation}}{Select the best \code{100*truncFactor} percent 
    of the best individuals of the population. The parameter \code{truncFactor} 
    specifies the truncation factor (default value: \code{0.3}).}
}
}

\value{
An \code{integer} vector with the indexes of the selected individuals from \code{pop}.
}
