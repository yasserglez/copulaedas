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

setClass("CEDA", 
    contains = "EDA",
    prototype = prototype(
        name = "Copula Estimation of Distribution Algorithm"))


learningCEDA <- function(eda, currGen, oldModel, selectedPop, selectedEval) {
  fmargin <- eda@learningOpts$fmargin
  pmargin <- eda@learningOpts$pmargin
  copula <- eda@learningOpts$copula
  fitMethod <- eda@learningOpts$fitMethod
  fitOptimMethod <- eda@learningOpts$fitOptimMethod
  fitOptimControl <- eda@learningOpts$fitOptimControl
  
  if (is.null(fmargin)) fmargin <- fnorm
  if (is.null(pmargin)) pmargin <- pnorm
  if (is.null(copula)) copula <- normalCopula(0)
  if (is.null(fitMethod)) fitMethod <- "itau"
  if (is.null(fitOptimMethod)) fitOptimMethod <- "Nelder-Mead"
  if (is.null(fitOptimControl)) fitOptimControl <- list(NULL)

  n <- ncol(selectedPop)
  margins <- lapply(seq(length = n),
      function (i) fmargin(selectedPop[ , i]))
  U <- sapply(seq(length = n),
      function (i) do.call(pmargin, c(list(selectedPop[ , i]), margins[[i]])))
  copula@dimension <- n
  copula <- fitCopula(copula, U, method = fitMethod, 
      optim.method = fitOptimMethod, optim.control = fitOptimControl, 
      estimate.variance = FALSE)@copula
  
  list(copula = copula, margins = margins)
}

setMethod("learning", "CEDA", learningCEDA)


samplingCEDA <- function (eda, currGen, model, popSize, lower, upper) {
  qmargin <- eda@learningOpts$qmargin
  
  if (is.null(qmargin)) qmargin <- qnorm
  
  U <- rcopula(model$copula, popSize)
  pop <- sapply(seq(length = ncol(U)),
      function (i) do.call(qmargin, c(list(U[ , i]), model$margins[[i]])))
  
  pop
}

setMethod("sampling", "CEDA", samplingCEDA)
