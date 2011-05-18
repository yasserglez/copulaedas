\name{VEDA-class}

\docType{class}

\alias{VEDA}
\alias{VEDA-class}
\alias{edaLearnVEDA}
\alias{edaSampleVEDA}
\alias{edaLearn,VEDA-method}
\alias{edaSample,VEDA-method}

\title{Class for Vine Estimation of Distribution Algorithms}

\description{
Extends the \code{\linkS4class{EDA}} class to implement Vine Estimation of
Distribution Algorithms.
}

\section{Objects from the Class}{
Objects are created by calls to the \code{VEDA} function.
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
Vine Estimation of Distribution Algorithms (VEDAs) are a class of EDAs introduced
in (Soto and González-Fernández, 2010) which model the search distributions
using vines. Vines are graphical models that build high-dimensional distributions 
by decomposing a multivariate density into bivariate copulas and one-dimensional 
densities (Joe, 1996; Bedford and Cooke, 2001; Aas et al., 2009). Similarly 
to Copula Estimation of Distribution Algorithms, these algorithms separately 
estimate the marginal distributions and the dependence structure between the 
variables of the selected population at each generation. Instead of representing 
the dependence structure using a single multivariate copula, VEDAs use a cascade
of bivariate copulas. Each bivariate copula can belong to different families. 
The following instances of VEDAs are implemented in the package.

\itemize{
\item{C-vine Estimation of Distribution Algorithm (CVEDA), which models
  the search distributions using C-vines (Soto and González-Fernández, 2010).}
\item{D-vine Estimation of Distribution Algorithm (DVEDA), which models
  the search distributions using D-vines (Soto and González-Fernández, 2010).}
}

Greedy heuristics are used to determine the structure of the C-vine and 
D-vine in CVEDA and DVEDA, respectively. The selection of each bivariate
copula in both decomposition starts with an independence test. The 
independence copula is selected if there is not enough evidence against
the null hypothesis of independence at a given significance level. In the 
other case, the parameters of a group candidate copulas are estimated and the 
one that better fits the data is selected. A model selection procedure based 
on AIC is used to truncate the vines if enough dependence is captured in the 
first trees. See the references and the documentation of the 
\code{\link[=vines-package]{vines}} \R package for more information 
about these matters.

The following parameters are recognized by the functions implementing the
\code{\link{edaLearn}} and \code{\link{edaSample}} methods for the
\code{\linkS4class{VEDA}} class.

\describe{
  \item{\code{vine}}{Vine type. Supported values are: \code{"CVine"} 
    (Canonical vine) and \code{"DVine"} (D-vine). Default value: \code{"DVine"}.}
  
  \item{\code{copulas}}{A \code{character} vector specifying the candidate copulas. 
    Supported values are: \code{"normal"} (normal copula),
    \code{"t"} (t copula), \code{"clayton"} (Clayton copula), and
    \code{"gumbel"} (Gumbel copula). Default value: \code{c("normal")}.}
  
  \item{\code{indepTestSigLevel}}{Significance level of the independence test. 
    Default value: \code{0.01}.}
      
  \item{\code{fmargin}}{Function to fit the marginal distributions. 
    Default value: \code{\link{fempirical}}.}
  
  \item{\code{pmargin}}{Cumulative distribution function of the marginal 
    distributions. Default value: \code{\link{pempirical}}.}
  
  \item{\code{qmargin}}{Inverse of the cumulative distribution function of 
    the marginal distributions. Default value: \code{\link{qempirical}}.}
  
  \item{\code{popSize}}{Population size. Default value: \code{100}.}
}
}

\references{
Aas, K. and Czado, C. and Frigessi, A. and Bakken, H. (2009). Pair-copula 
constructions of multiple dependence. \emph{Insurance: Mathematics and 
Economics}, 44:182--198.

Bedford, T. and Cooke, R. M. (2001). Probability density decomposition for 
conditionally dependent random variables modeled by vines. \emph{Annals 
of Mathematics and Artificial Intelligence}, 32:245--268.

Joe, H. (1996). Families of \eqn{m}-variate distributions with given margins 
and \eqn{m(m-1)/2} bivariate dependence parameters. In Rüschendorf, L. 
and Schweizer, B. and Taylor, M. D., editors, \emph{Distributions with 
fixed marginals and related topics}, 120--141.

Soto, M. and González-Fernández, Y. (2010). Vine Estimation of Distribution
Algorithms, Technical Report ICIMAF 2010-561, Institute of Cybernetics,
Mathematics, and Physics.
}

\examples{
setMethod("edaTerminate", "EDA", edaTerminateEval)
setMethod("edaReport", "EDA", edaReportSimple)

CVEDA <- VEDA(vine = "CVine",
              copulas = c("normal", "clayton", "gumbel"),
              indepTestSigLevel = 0.01, fmargin = fnorm,
              pmargin = pnorm, qmargin = qnorm, popSize = 200,
              fEval = 0, fEvalTol = 1e-03)
CVEDA@name <- "C-vine Estimation of Distribution Algorithm"

DVEDA <- VEDA(vine = "DVine",
              copulas = c("normal", "clayton", "gumbel"),
              indepTestSigLevel = 0.01, fmargin = fnorm,
              pmargin = pnorm, qmargin = qnorm, popSize = 200,
              fEval = 0, fEvalTol = 1e-03)
DVEDA@name <- "D-vine Estimation of Distribution Algorithm"

resultsCVEDA <- edaRun(CVEDA, fSphere, rep(-600, 5), rep(600, 5))
resultsDVEDA <- edaRun(DVEDA, fSphere, rep(-600, 5), rep(600, 5))

show(resultsCVEDA)
show(resultsDVEDA)
}

\keyword{classes}
