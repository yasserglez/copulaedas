\name{margin-kernel}

\alias{fkernel}
\alias{pkernel}
\alias{qkernel}

\title{Kernel-Smoothed Empirical Marginal Distribution}

\description{
Functions that implement the kernel-smoothed empirical marginal 
distributions for EDAs with the \code{margin} parameter set to 
\code{"kernel"}.
}

\usage{
fkernel(x, lower, upper)
pkernel(q, X, h)
qkernel(p, X, h)
}

\arguments{
  \item{x, q}{Vector of quantiles.}
  
  \item{lower}{Lower bound of the variable.}
  
  \item{upper}{Upper bound of the variable.}
  
  \item{p}{Vector of probabilities}
  
  \item{X}{Observations of the variable.}
  
  \item{h}{Bandwidth of the kernel.}
}

\details{
The \code{fkernel} function fits the marginal distribution. It returns a
\code{list} object with the observations of the variable (\code{X} component) 
and the bandwidth of a Gaussian kernel density estimator (\code{h} component). 
The bandwidth is calculated using Silverman's rule of thumb (see 
\code{\link{bw.nrd0}}). The components of the \code{list} object returned
by \code{fkernel} are used as aditional arguments in the \code{pkernel}
and \code{qkernel} functions.

The \code{pkernel} function calculates the empirical cumulative distribution 
function. The expression of the empirical cumulative distribution function 
includes the modification used in the copula context to avoid problems 
in the boundary of the \eqn{[0,1]} interval.

The \code{qkernel} function uses the Gaussian kernel density estimator
fitted by \code{fkernel} to evaluate the inverse of the cumulative
distribution function, following the procedure suggested in (Azzalini, 1981).
}

\references{
Azzalini, A. (1981) A note on the estimation of a distribution function and 
quantiles by a kernel method, \emph{Biometrika}, \bold{68}, 326-328.
}
