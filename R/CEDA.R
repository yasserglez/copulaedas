# edas: R package for Estimation of Distribution Algorithms
# Copyright (C) 2010, 2011 Yasser González-Fernández
# Copyright (C) 2010, 2011 Marta Soto
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


CEDA <- function (...) {
    new("CEDA", parameters = list(...))
}


edaLearnCEDA <- function(eda, gen, previousModel,
        selectedPop, selectedEval, lower, upper) {
    fmargin <- eda@parameters$fmargin
    pmargin <- eda@parameters$pmargin
    copula <- eda@parameters$copula

    fitCopulaArgs <- as.list(eda@parameters$fitCopulaArgs)

    if (is.null(fmargin)) fmargin <- fempirical
    if (is.null(pmargin)) pmargin <- pempirical
    if (is.null(copula)) copula <- "normal"

    n <- ncol(selectedPop)
    margins <- lapply(seq(length = n),
            function (i) fmargin(selectedPop[ , i], lower[i], upper[i]))
    U <- sapply(seq(length = n),
            function (i) do.call(pmargin, c(list(selectedPop[ , i]), margins[[i]])))

    copula <- switch(copula,
            indep = indepCopula(dim = n),
            normal = normalCopula(rep(0, choose(n, 2)), dim = n, dispstr = "un"))
    if (length(copula@parameters) > 0) {
        copula <- fitCopula(copula = copula, data = U,
                method = "itau", estimate.variance = FALSE)@copula
    }

    list(copula = copula, margins = margins)
}

setMethod("edaLearn", "CEDA", edaLearnCEDA)


edaSampleCEDA <- function (eda, gen, model, lower, upper) {
    popSize <- eda@parameters$popSize
    qmargin <- eda@parameters$qmargin

    if (is.null(popSize)) popSize <- 100
    if (is.null(qmargin)) qmargin <- qempirical

    U <- rcopula(model$copula, popSize)

    pop <- sapply(seq(length = ncol(U)),
            function (i) do.call(qmargin, c(list(U[ , i]), model$margins[[i]])))

    matrix(pop, nrow = popSize)
}

setMethod("edaSample", "CEDA", edaSampleCEDA)
