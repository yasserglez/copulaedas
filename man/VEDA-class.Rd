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
Extends the \code{\linkS4class{EDA}} class to implement Estimation of
Distribution Algorithms based on vines.
}

\section{Objects from the Class}{
Objects are created by calling the \code{VEDA} function.
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
  \item{edaLearn}{\code{signature(eda = "CEDA")}: The \code{edaLearnCEDA} 
    function.}
  \item{edaSample}{\code{signature(eda = "CEDA")}: The \code{edaSampleCEDA} 
    function.}
}
}

\details{
Vine Estimation of Distribution Algorithms (VEDA) are a class of EDAs (Soto 
and González-Fernández, 2010; González-Fernández, 2011) that model the search
distributions using vines. Vines are graphical models that represent
high-dimensional distributions by decomposing the multivariate density
into bivariate copulas and one-dimensional densities (Joe, 1996; 
Bedford and Cooke, 2001; Aas et al., 2009). Similarly to Copula Estimation 
of Distribution Algorithms, these algorithms estimate separately the 
univariate marginal distributions and the dependence structure from the 
selected population. Instead of representing the dependence structure using 
a single multivariate copula, VEDAs model a rich variety of dependencies by 
combining bivariate copulas that belong to different families. The following 
instances of VEDA are implemented.

\itemize{
\item{C-vine Estimation of Distribution Algorithm (CVEDA), that models
  the search distributions using C-vines (Soto and González-Fernández, 2010; 
  González-Fernández, 2011).}
\item{D-vine Estimation of Distribution Algorithm (DVEDA), that models
  the search distributions using D-vines (Soto and González-Fernández, 2010; 
  González-Fernández, 2011).}
}

Greedy heuristics based on the empirical Kendall's tau between each variable
in the selected population are used to determine the structure of the C-vine 
and D-vine in CVEDA and DVEDA, respectively (Brechmann, 2010). 

The selection of each bivariate copula in both decompositions starts with 
an independence test (Genest and Rémillard, 2004; Genest et al., 2007). 
The independence copula is selected if there is not enough evidence against 
the null hypothesis of independence at a given significance level. In the 
other case, the parameters of a group of candidate copulas are estimated 
and the one that minimizes a distance to the empirical copula is selected.
As distance we use a Cramér-von Mises statistic (Genest and Rémillard, 2008).

The parameters of all the candidate copulas but the t copula are estimated
using the inversion of Kendall's tau. In the case of the t copula,
the correlation coeffcient is computed using the inversion of Kendall's tau
and the degrees of freedom are estimated by maximum likelihood with the 
correlation parameter fixed (Demarta and McNeil, 2005).

Besides, a model selection procedure based on Akaike Information Criterion 
is used to truncate the vines if enough dependence is captured in the first 
trees (Brechmann, 2010).

The following parameters are recognized by the functions that implement 
the \code{\link{edaLearn}} and \code{\link{edaSample}} methods for the
\code{\linkS4class{VEDA}} class.

\describe{
  \item{\code{vine}}{Vine type. Supported values are: \code{"CVine"}
    (Canonical vine) and \code{"DVine"} (D-vine). Default value:
    \code{"DVine"}.}

  \item{\code{copulas}}{A \code{character} vector specifying the candidate
    copulas. Supported values are: \code{"normal"} (normal copula),
    \code{"t"} (t copula), \code{"clayton"} (Clayton copula), and
    \code{"gumbel"} (Gumbel copula). Default value: \code{c("normal")}.}

  \item{\code{indepTestSigLevel}}{Significance level of the independence
    test. Default value: \code{0.01}.}

  \item{\code{margin}}{Marginal distributions. If the value of this argument
    is \code{"xxx"}, the algorithm will search for three functions named
    \code{fxxx}. \code{pxxx} and \code{qxxx} to fit each marginal distribution
    and evaluate the cumulative distribution function and its inverse,
    respectively. Default value: \code{"norm"}.}

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

Brechmann, E. C. (2010) Truncated and simplified regular vines 
and their applications. Diploma thesis. University of Technology, 
Munich, Germany.

Demarta, S. and McNeil, A. J. (2005). The t copula and related copulas, 
\emph{International Statistical Review}, 73:111--129.

Genest, C. and Rémillard, B. (2004). Tests of independence or randomness 
based on the empirical copula process. Test, 13:335--369.

Genest, C. and Quessy, J. F. and Rémillard, B. (2007). Asymptotic local 
efficiency of Cramér-von Mises tests for multivariate independence.
The Annals of Statistics, 35:166--191.

Genest, C. and Rémillard, B. (2008). Validity of the parametric bootstrap 
for goodness-of-fit testing in semiparametric models. Annales de l'Institut 
Henri Poincaré: Probabilités et Statistiques, 44:1096--1127.

González-Fernández, Y. (2011). Algoritmos con estimación de distribuciones
basados en cópulas y vines. Bachelor thesis. University of Havana, Cuba.

Joe, H. (1996). Families of \eqn{m}-variate distributions with given margins
and \eqn{m(m-1)/2} bivariate dependence parameters. In Röschendorf, L.
and Schweizer, B. and Taylor, M. D., editors, \emph{Distributions with
fixed marginals and related topics}, 120--141.

Soto, M. and González-Fernández, Y. (2010). Vine Estimation of Distribution
Algorithms. Technical Report ICIMAF 2010-561. ISSN 0138-8916. Institute of 
Cybernetics, Mathematics and Physics, Cuba.
}

\examples{
setMethod("edaTerminate", "EDA", edaTerminateEval)
setMethod("edaReport", "EDA", edaReportSimple)

CVEDA <- VEDA(vine = "CVine",
    copulas = c("normal", "clayton", "gumbel"),
    indepTestSigLevel = 0.01, margin = "norm",
    popSize = 200, fEval = 0, fEvalTol = 1e-03)
CVEDA@name <- "C-vine Estimation of Distribution Algorithm"

DVEDA <- VEDA(vine = "DVine",
    copulas = c("normal", "clayton", "gumbel"),
    indepTestSigLevel = 0.01, margin = "norm",
    popSize = 200, fEval = 0, fEvalTol = 1e-03)
DVEDA@name <- "D-vine Estimation of Distribution Algorithm"

resultsCVEDA <- edaRun(CVEDA, fSphere, rep(-600, 5), rep(600, 5))
resultsDVEDA <- edaRun(DVEDA, fSphere, rep(-600, 5), rep(600, 5))

show(resultsCVEDA)
show(resultsDVEDA)
}

\keyword{classes}
