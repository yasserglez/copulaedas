\name{margin-empirical}

\alias{fempirical}
\alias{pempirical}
\alias{qempirical}

\title{Empirical Marginal Distribution}

\description{
These functions can be set as values of the \code{fmargin}, \code{pmargin},
and \code{qmargin} parameters of the EDAs to use empirical marginal
distributions.
}

\usage{
fempirical(x, lower, upper)
pempirical(q, X, h)
qempirical(p, X, h)
}

\arguments{
\item{x, q}{vector of quantiles.}
\item{lower}{lower bound of the variable.}
\item{upper}{upper bound of the variable.}
\item{p}{vector of probabilities}
\item{X}{observations of the variable.}
\item{h}{bandwidth of the kernel.}
}

\details{
The \code{fempirical} function fits the marginal distribution. It returns a
\code{"list"} object with the observations of the variable (\code{X} component) 
and the bandwidth of a Gaussian kernel density estimator (\code{h} component). 
The bandwidth is calculated using Silverman's rule of thumb (see 
\code{\link{bw.nrd0}}). The components of the \code{"list"} object returned
by \code{fempirical} are used as aditional arguments in the \code{pempirical} 
and \code{qempirical} functions.

The \code{pempirical} function calculates the empirical cumulative distribution 
function. The expression of the empirical cumulative distribution function 
includes the modification used in the copula context to avoid problems 
in the boundary of the \eqn{[0,1]} interval.

The \code{qempirical} function uses the Gaussian kernel density estimator
fitted by \code{fempirical} to evaluate the inverse of the cumulative
distribution function, following the procedure suggested in (Azzalini, 1981).


}

\references{
Azzalini, A. (1981) A note on the estimation of a distribution function and 
quantiles by a kernel method, \emph{Biometrika}, \bold{68}, 326-328.
}
