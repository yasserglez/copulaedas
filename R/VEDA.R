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
  fitMethod <- eda@options$fitMethod
  vine <- eda@options$vine
  trees <- eda@options$trees
  copulas <- eda@options$copulas
  corTestMethod <- eda@options$corTestMethod
  corTestSigLevel <- eda@options$corTestSigLevel
  gofCopulaIters <- eda@options$gofCopulaIters 
  gofCopulaMethod <- eda@options$gofCopulaMethod 
  gofCopulaSimul <- eda@options$gofCopulaSimul
  fitOptimMethod <- eda@options$fitOptimMethod
  fitOptimControl <- eda@options$fitOptimControl  
  
  if (is.null(fmargin)) fmargin <- fnorm
  if (is.null(pmargin)) pmargin <- pnorm
  if (is.null(fitMethod)) fitMethod <- "ml"
  if (is.null(vine)) vine <- "DVine"
  if (is.null(trees)) trees <- ncol(selectedPop) - 1
  if (is.null(copulas)) copulas <- list(normalCopula(0), tCopula(0), 
        claytonCopula(1), gumbelCopula(1))
  if (is.null(corTestMethod)) corTestMethod <- "kendall"
  if (is.null(corTestSigLevel)) corTestSigLevel <- 0.1
  if (is.null(gofCopulaIters)) gofCopulaIters <- 1000
  if (is.null(gofCopulaMethod)) gofCopulaMethod <- "itau"
  if (is.null(gofCopulaSimul)) gofCopulaSimul <- "mult"
  if (is.null(fitOptimMethod)) fitOptimMethod <- NULL
  if (is.null(fitOptimControl)) fitOptimControl <- list()

  n <- ncol(selectedPop)
  margins <- lapply(seq(length = n), 
      function (i) fmargin(selectedPop[ , i]))
  U <- sapply(seq(length = n),
      function (i) do.call(pmargin, c(list(selectedPop[ , i]), margins[[i]])))
  vine <- fitVine(vine, U, method = fitMethod, trees = trees, copulas = copulas, 
      corTestMethod = corTestMethod, corTestSigLevel = corTestSigLevel,
      gofCopulaIters = gofCopulaIters, gofCopulaMethod = gofCopulaMethod, 
      gofCopulaSimul = gofCopulaSimul, optimMethod = fitOptimMethod, 
      optimControl = fitOptimControl)@vine

  list(vine = vine, margins = margins)
}

setMethod("learning", "VEDA", learningVEDA)


samplingVEDA <- function (eda, currGen, model, lower, upper) {
  popSize <- eda@options$popSize
  qmargin <- eda@options$qmargin
  
  if (is.null(popSize)) popSize <- 100
  if (is.null(qmargin)) qmargin <- qnorm
  
  U <- rvine(model$vine, popSize)
  pop <- sapply(seq(length = ncol(U)),
      function (i) do.call(qmargin, c(list(U[ , i]), model$margins[[i]])))
  
  pop
}

setMethod("sampling", "VEDA", samplingVEDA)
