\name{CEDA-class}

\docType{class}

\alias{CEDA}
\alias{CEDA-class}
\alias{edaLearnCEDA}
\alias{edaSampleCEDA}
\alias{edaLearn,CEDA-method}
\alias{edaSample,CEDA-method}

\title{Class for Copula Estimation of Distribution Algorithms}

\description{
Extends the \code{\linkS4class{EDA}} class to implement Copula Estimation 
of Distribution Algorithms.
}

\section{Objects from the Class}{
Objects are created by calls to the \code{CEDA} function.
}

\section{Slots}{
\describe{
  \item{\code{name}:}{See the documentation of the slot in the
  	\code{\linkS4class{EDA}} class.}
  \item{\code{parameters}:}{See the documentation of the slot in the
  	\code{\linkS4class{EDA}} class.}
}
}

\section{Methods}{
\describe{
  \item{edaLearn}{\code{signature(eda = "CEDA")}:
    The \code{edaLearnCEDA} function.}
  \item{edaSample}{\code{signature(eda = "CEDA")}:
    The \code{edaSampleCEDA} function.}
}
}

\details{
Copula Estimation of Distribution Algorithms are a class of EDAs which model 
the search distributions using multivariate copulas. These algorithms 
separately estimate the marginal distributions and the dependence structure 
between the variables of the selected population at each generation. The 
dependence structure is represented by a multivariate copula. The following 
instances of this class of EDAs are implemented in the package.

\itemize{
\item{If the dependence structure is modeled using the independence copula, 
  the resulting algorithm corresponds to the continuous Univariate Marginal 
  Distribution Algorithm (UMDA) introduced in (Larrañaga et al., 2000).}

\item{If the dependence structure is modeled using the normal copula, the
  resulting algorithm corresponds to the Gaussian Copula Estimation of
  Distribution Algorithm (GCEDA) introduced in (Soto et al., 2007; Arderí, 2007). 
  The correlation matrix of the normal copula is calculated using the inversion
  of Kendall's tau for each pair of variables (Demarta and McNeil, 2005).
  If the resulting correlation matrix is not positive-definite, the correction
  proposed in (Rousseeuw and Molenberghs, 1993) is applied.}
}

The following parameters are recognized by the functions implementing the
\code{\link{edaLearn}} and \code{\link{edaSample}} methods for the
\code{\linkS4class{CEDA}} class.

\describe{
\item{\code{fmargin}}{Function to fit the marginal distributions. 
  Default value: \code{\link{fempirical}}.}
\item{\code{pmargin}}{Cumulative distribution function of the marginal 
  distributions. Default value: \code{\link{pempirical}}.}
\item{\code{qmargin}}{Inverse of the cumulative distribution function of 
  the marginal distributions. Default value: \code{\link{qempirical}}.}
\item{\code{copula}}{Multivariate copula used to represent the dependence 
  structure. Supported values are: \code{"indep"} (independence copula) and 
  \code{"normal"} (normal copula). Default value: \code{"normal"}.}
\item{\code{popSize}}{Population size. Default value: \code{100}.}
}
}

\references{
Arderí, R. J. (2007). Algoritmo con Estimación de Distribuciones con Cópula
Gaussiana, Diploma Thesis, University of Havana.

Demarta, S. and McNeil, A. J. (2005). The t copula and related copulas, 
\emph{International Statistical Review}, 73:111--129.

Larrañaga, P. and Etxeberria, R. and Lozano, J. A. and Peña, J. M. (2000).
Optimization in continous domains by learning and simulation of Gaussian 
networks. \emph{Proceedings of the Workshop in Optimization by Building and 
Using Probabilistic Models in the Genetic and Evolutionary Computation 
Conference (GECCO 2000)}, 201--204.

Rousseeuw, P. and Molenberghs, G. (1993). Transformation of nonpositive 
semidefinite correlation matrices. \emph{Communications in Statistics: 
Theory and Methods}, 22:965--984.

Soto, M. and Ochoa, A. and Arderí, R. J. (2007). El Algoritmo con Estimación
de Distribuciones basado en Cópula Gausiana, Technical Report ICIMAF 2007-406,
Institute of Cybernetics, Mathematics, and Physics.
}

\examples{
setMethod("edaTerminate", "EDA", edaTerminateEval)
setMethod("edaReport", "EDA", edaReportSimple)

UMDA <- CEDA(copula = "indep", fmargin = fnorm, 
             pmargin = pnorm, qmargin = qnorm,
             popSize = 200, fEval = 0, fEvalTol = 1e-03)
UMDA@name <- "Univariate Marginal Distribution Algorithm"

GCEDA <- CEDA(copula = "normal", fmargin = fnorm, 
              pmargin = pnorm, qmargin = qnorm,
              popSize = 200, fEval = 0, fEvalTol = 1e-03)
GCEDA@name <- "Gaussian Copula Estimation of Distribution Algorithm"

resultsUMDA <- edaRun(UMDA, fSphere, rep(-600, 5), rep(600, 5))
resultsGCEDA <- edaRun(GCEDA, fSphere, rep(-600, 5), rep(600, 5))

show(resultsUMDA)
show(resultsGCEDA)
}

\keyword{classes}
