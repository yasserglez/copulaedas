\name{edaSeed}

\alias{edaSeed}
\alias{edaSeedUniform}

\title{Seeding Methods}

\description{
Methods for the \code{edaSeed} generic function.
}

\usage{
edaSeedUniform(eda, lower, upper)
}

\arguments{
  \item{eda}{\code{\linkS4class{EDA}} instance.}
  
  \item{lower}{Lower bounds of the variables of the objective function.}
  
  \item{upper}{Upper bounds of the variables of the objective function.}
}

\details{
Seeding methods create the initial population. The length of the \code{lower}
and \code{upper} vectors determine the number of variables of the objective 
function. The following seeding methods are implemented.

\describe{
  \item{\code{edaSeedUniform}}{Sample each variable from a continuous uniform
    distribution in the interval determined by the \code{lower} and 
    \code{upper} vectors. The parameter \code{popSize} sets the number of
    candidate solutions in the population (default value: \code{100}).}
}
}

\value{
A matrix with one column for each variable of the objective function and one
row for each candidate solution in the population.
}
