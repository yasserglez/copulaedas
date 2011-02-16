# edas: R package for Estimation of Distribution Algorithms
# Copyright (C) 2010-2011 Yasser González-Fernández
# Copyright (C) 2010-2011 Marta Soto
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

setClass("UMDA", 
        contains = "EDA",
        prototype = prototype(
                name = "Univariate Marginal Distribution Algorithm"))


UMDA <- function (parameters = list()) {
    new("UMDA", parameters = parameters)
}


learningUMDA <- function(eda, currGen, oldModel, selectedPop, selectedEval) {
    fmargin <- eda@parameters$fmargin
    
    if (is.null(fmargin)) fmargin <- fspline
    
    margins <- lapply(seq(length = ncol(selectedPop)),
            function (i) fmargin(selectedPop[ , i]))
    
    list(margins = margins)
}

setMethod("learning", "UMDA", learningUMDA)


samplingUMDA <- function (eda, currGen, model, lower, upper) {
    popSize <- eda@parameters$popSize
    qmargin <- eda@parameters$qmargin
    
    if (is.null(popSize)) popSize <- 100
    if (is.null(qmargin)) qmargin <- qspline
    
    n <- length(lower)
    U <- matrix(runif(popSize * n), popSize, n)
    pop <- sapply(seq(length = n),
            function (i) do.call(qmargin, c(list(U[ , i]), model$margins[[i]])))
    
    pop
}

setMethod("sampling", "UMDA", samplingUMDA)
