\name{problems}

\alias{fAckley}
\alias{fGriewank}
\alias{fRosenbrock}
\alias{fSphere}
\alias{fSummationCancellation}

\title{Test Problems}

\description{
A group of well-known continuous optimization test problems.
}

\usage{
fAckley(x)
fGriewank(x)
fRosenbrock(x)
fSphere(x)
fSummationCancellation(x)
}

\arguments{
\item{x}{A vector to be evaluated in the function.}
}

\value{
The value of the function for the vector \code{x}.
}

\details{
The definition of the functions for a vector 
\eqn{\boldsymbol{x}=(x_{1},\ldots,x_{n})} is given below.

\deqn{\texttt{fAckley}(\boldsymbol{x})=-20\exp\left(-0.2\sqrt{\frac{1}{n}\sum_{i=1}^{n}x^{2}}\right)-\exp\left(\frac{1}{n}\sum_{i=1}^{n}\cos\left(2\pi x_{i}\right)\right)+20+\exp\left(1\right)}

\deqn{\texttt{fGriewank}(\boldsymbol{x})=1+\sum_{i=1}^{n}\frac{x_{i}^{2}}{4000}-\prod_{i=1}^{n}\cos\left(\frac{x_{i}}{\sqrt{i}}\right)}

\deqn{\texttt{fRosenbrock}(\boldsymbol{x})=\sum_{i=1}^{n-1}\left(100\left(x_{i+1}-x_{i}^{2}\right)^{2}+\left(1-x_{i}\right)^{2}\right)}

\deqn{\texttt{fSphere}(\boldsymbol{x})=\sum_{i=1}^{n}x_{i}^{2}}

\deqn{\texttt{fSummationCancellation}(\boldsymbol{x})=\frac{1}{10^{-5}+\sum_{i=1}^{n}|y_{i}|},\, y_{1}=x_{1},\, y_{i}=y_{i-1}+x_{i}}

Ackley, Griewank, Rosenbrock, and Sphere are minimization problems. 
Summation Cancellation is originally a maximization problem but it is
expressed here as a minimization problem. Sphere, Griewank, and Ackley have 
their global optimum at \eqn{\boldsymbol{x}=(0,\ldots,0)} with evaluation 0.
Rosenbrock has its global optimum at \eqn{\boldsymbol{x}=(1,\ldots,1)} with
evaluation 0. Summation Cancellation has its global optimum at 
\eqn{\boldsymbol{x}=(0,\ldots,0)} with evaluation \eqn{-10^{5}}.
}

\examples{
all.equal(fAckley(rep(0, 10)), 0)
all.equal(fGriewank(rep(0, 10)), 0)
all.equal(fRosenbrock(rep(1, 10)), 0)
all.equal(fSphere(rep(0, 10)), 0)
all.equal(fSummationCancellation(rep(0, 10)), -1e+05)
}

\references{
Bengoetxea, E. and Miquélez, T. and Lozano, J. A. and Larrañaga, P.
(2002) Experimental results in function optimization with EDAs in 
continuous domain. In Larrañaga, P. and Lozano, J. A., editors, 
\emph{Estimation of Distribution Algorithms. A new tool for Evolutionary
Computation}, 181--194. Kluwer Academic Publisher.
}
