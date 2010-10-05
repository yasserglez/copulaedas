# edas: GNU R package for Estimation of Distribution Algorithms
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

setClass("VEDA",
    contains = "EDA",
    prototype = prototype(
        name = "Vine Estimation of Distribution Algorithm"))


learningVEDA <- function(eda, currGen, oldModel, selectedPop, selectedEval) {
  fmargin <- eda@options$fmargin
  pmargin <- eda@options$pmargin
  orderingMethod <- eda@options$orderingMethod
  orderingArgs <- eda@options$orderingArgs
  type <- eda@options$type
  fitMethod <- eda@options$fitMethod
  fitArgs <- eda@options$fitArgs
  
  if (is.null(fmargin)) fmargin <- fnorm
  if (is.null(pmargin)) pmargin <- pnorm
  if (is.null(orderingMethod)) orderingMethod <- "greedy"
  if (is.null(orderingArgs)) orderingArgs <- list(according = "kendall")
  if (is.null(type)) type <- "DVine"
  if (is.null(fitMethod)) fitMethod <- "ml"
  if (is.null(fitArgs)) fitArgs <- list(
        trees = ncol(selectedPop) - 1,
        copulas = list(normalCopula(0), tCopula(0), 
            claytonCopula(1), gumbelCopula(1)),
        corTestMethod = "kendall",
        corTestSigLevel = 0.1,
        gofCopulaIters = 1000,
        gofCopulaMethod = "itau",
        gofCopulaSimul = "mult",
        optimMethod = NULL,
        optimControl = list())
  
  n <- ncol(selectedPop)
  margins <- lapply(seq(length = n), 
      function (i) fmargin(selectedPop[ , i]))
  uniformPop <- sapply(seq(length = n),
      function (i) do.call(pmargin, c(list(selectedPop[ , i]), margins[[i]])))
  ordering <- do.call(orderingVine,
      c(list(type = type, data = uniformPop, method = orderingMethod), orderingArgs))
  orderedPop <- uniformPop[ , ordering]
  vine <- do.call(fitVine,
      c(list(type = type, data = orderedPop, method = fitMethod), fitArgs))@vine

  list(margins = margins, ordering = ordering, vine = vine)
}

setMethod("learning", "VEDA", learningVEDA)


samplingVEDA <- function (eda, currGen, model, lower, upper) {
  popSize <- eda@options$popSize
  qmargin <- eda@options$qmargin
  
  if (is.null(popSize)) popSize <- 100
  if (is.null(qmargin)) qmargin <- qnorm

  orderedPop <- rvine(model$vine, popSize)
  uniformPop <- matrix(NA, nrow(orderedPop), ncol(orderedPop))
  for (k in seq(length = ncol(orderedPop))) {
    uniformPop[ , model$ordering[k]] <- orderedPop[ , k]
  }
  finalPop <- sapply(seq(length = ncol(uniformPop)),
      function (i) do.call(qmargin, 
            c(list(uniformPop[ , i]), model$margins[[i]])))
  
  finalPop
}

setMethod("sampling", "VEDA", samplingVEDA)
