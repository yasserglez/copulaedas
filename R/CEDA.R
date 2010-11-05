# edas: R package for Estimation of Distribution Algorithms
# Copyright (C) 2010 Yasser González Fernández <ygonzalezfernandez@gmail.com>
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
# details.
#
# You should have received a copy of the GNU General Public License along with 
# this program. If not, see <http://www.gnu.org/licenses/>.

setClass("CEDA", 
    contains = "EDA",
    prototype = prototype(
        name = "Copula Estimation of Distribution Algorithm"))


learningCEDA <- function(eda, currGen, oldModel, selectedPop, selectedEval) {
  fmargin <- eda@parameters$fmargin
  pmargin <- eda@parameters$pmargin
  fitCopulaArgs <- as.list(eda@parameters$fitCopulaArgs)

  if (is.null(fmargin)) fmargin <- fnorm
  if (is.null(pmargin)) pmargin <- pnorm
  fitCopulaArgs <- updateList(
      list(copula = normalCopula(0),
           method = "ml",
           estimate.variance = FALSE),
      fitCopulaArgs)
  
  n <- ncol(selectedPop)
  margins <- lapply(seq(length = n),
      function (i) fmargin(selectedPop[ , i]))
  U <- sapply(seq(length = n),
      function (i) do.call(pmargin, c(list(selectedPop[ , i]), margins[[i]])))
  # Set the dimension of the copula to the dimension of the population.
  copula <- fitCopulaArgs$copula
  if (copula@dimension != n) {
    copula <- switch(class(copula),
        claytonCopula = claytonCopula(copula@parameters, n),
        gumbelCopula = gumbelCopula(copula@parameters, n),
        frankCopula = frankCopula(copula@parameters, n),
        normalCopula = normalCopula(rep(0, choose(n, 2)), n, dispstr = "un"),
        tCopula = tCopula(rep(0, choose(n, 2)), n, dispstr = "un", df = copula@df))
  }
  # Set the start argument for the t copula without df fixed.
  start <- if (is(copula, "tCopula") && !copula@df.fixed) copula@parameters else NULL
  copula <- do.call(fitCopula,
      updateList(fitCopulaArgs, list(copula = copula, data = U, start = start)))@copula

  list(copula = copula, margins = margins)
}

setMethod("learning", "CEDA", learningCEDA)


samplingCEDA <- function (eda, currGen, model, lower, upper) {
  popSize <- eda@parameters$popSize
  qmargin <- eda@parameters$qmargin

  if (is.null(popSize)) popSize <- 100
  if (is.null(qmargin)) qmargin <- qnorm

  U <- rcopula(model$copula, popSize)
  pop <- sapply(seq(length = ncol(U)),
      function (i) do.call(qmargin, c(list(U[ , i]), model$margins[[i]])))

  pop
}

setMethod("sampling", "CEDA", samplingCEDA)
