\name{edaSeed}

\alias{edaSeed}
\alias{edaSeedUniform}

\title{Seeding Methods}

\description{
Method definitions for the \code{edaSeed} generic function.
}

\usage{
edaSeedUniform(eda, lower, upper)
}

\arguments{
  \item{eda}{An \code{\linkS4class{EDA}} instance.}
  
  \item{lower}{Lower bounds of the variables of the objective function.}
  
  \item{upper}{Upper bounds of the variables of the objective function.}
}

\details{
Seeding methods are used to create the initial population of the EDA. The length
of the \code{lower} and \code{upper} vectors determine the number of variables
of the objective function. The following seeding methods are implemented.

\describe{
  \item{\code{edaSeedUniform}}{Sample each variable according to a continuous 
    uniform distribution in the interval determined by the \code{lower} and 
    \code{upper} vectors. The parameter \code{popSize} specifies the number 
    of individuals to be sampled (default value: \code{100}).}
}
}

\value{
A matrix object with one column for each variable of the objective function 
and one row for each individual in the population.
}
