\name{margin-norm}

\alias{fnorm}

\title{Normal Marginal Distribution}

\description{
Functions that implement the normal marginal distributions for EDAs
with the \code{margin} parameter set to \code{"norm"}.
}

\usage{
fnorm(x, lower, upper)
}

\arguments{
  \item{x}{Vector of quantiles.}

  \item{lower}{Lower bound of the variable.}

  \item{upper}{Upper bound of the variable.}
}

\details{
The \code{fnorm} function fits the marginal distribution. It returns a
\code{list} object with the mean (\code{mean} component) and the standard
deviation (\code{sd} component). These components give values to the
corresponding arguments to the \code{\link{pnorm}} and \code{\link{qnorm}}
functions.
}

\seealso{
\code{\link{pnorm}},
\code{\link{qnorm}}.
}
