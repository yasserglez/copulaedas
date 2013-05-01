\encoding{UTF-8}

\name{EDAResults-class}

\docType{class}

\alias{EDAResults-class}
\alias{show,EDAResults-method}
\alias{summary,EDAResults-method}

\title{Class for the Results of a Sequence of Runs of an EDA}

\description{
Results of a sequence of independent runs of an EDA. This class is just a 
wrapper for a \code{list} object containing \code{\linkS4class{EDAResult}} 
instances. Objects are created by calling the \code{\link{edaIndepRuns}}
function.
}

\section{Methods}{
\describe{
  \item{show}{\code{signature(object = "EDAResults")}: Prints a table with 
    the results.}

  \item{summary}{\code{signature(object = "EDAResults")}: Prints a summary 
    of the results.}
}
}

\references{
Gonzalez-Fernandez Y and Soto M (2013). \pkg{copulaedas}: An \R Package for
Estimation of Distribution Algorithms Based on Copulas. Preprint
\href{http://arxiv.org/abs/1209.5429}{arXiv:1209.5429 [cs.NE]}.
}

\seealso{
\code{\linkS4class{EDA}},
\code{\link{edaIndepRuns}}.
}

\keyword{classes}
