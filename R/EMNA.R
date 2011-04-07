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

setClass("EMNA",
        contains = c("EDA", "VIRTUAL"),
        prototype = prototype(
                name = "Estimation of Multivariate Normal Algorithm"))

setClass("EMNAg",
        contains = "EMNA",
        prototype = prototype(
                name = "Estimation of Multivariate Normal Algorithm global"))

setClass("EMNAa",
        contains = "EMNA",
        prototype = prototype(
                name = "Estimation of Multivariate Normal Algorithm adaptive"))

setClass("EMNAi",
        contains = "EMNA",
        prototype = prototype(
                name = "Estimation of Multivariate Normal Algorithm incremental"))


EMNAg <- function (parameters = list()) {
    new("EMNAg", parameters = parameters)
}

EMNAa <- function (parameters = list()) {
    new("EMNAa", parameters = parameters)
}

EMNAi <- function (parameters = list()) {
    new("EMNAi", parameters = parameters)
}


learningEMNA <- function(eda, currGen, oldModel, selectedPop, selectedEval, lower, upper) {
    list(mean = colMeans(selectedPop), sigma = cov(selectedPop))
}

setMethod("learning", "EMNA", learningEMNA)


samplingEMNAg <- function (eda, currGen, model, lower, upper) {
    popSize <- eda@parameters$popSize
    
    if (is.null(popSize)) popSize <- 100
    
    rmvnorm(popSize, model$mean, model$sigma)
}

setMethod("sampling", "EMNAg", samplingEMNAg)


samplingEMNAai <- function (eda, currGen, model, lower, upper) {
    popSize <- eda@parameters$popSize
    
    if (is.null(popSize)) popSize <- 100
    
    rmvnorm(1, model$mean, model$sigma)
}

setMethod("sampling", "EMNAa", samplingEMNAai)

setMethod("sampling", "EMNAi", samplingEMNAai)


replacementEMNAa  <- function (eda, currGen, pop, popEval, selectedPop,
        selectedEval, sampledPop, sampledEval) {
    if (any(sampledEval < popEval)) {
        i <- which.max(popEval)
        pop[i, ] <- sampledPop[1, ]
        popEval[i] <- sampledEval[1]    
    }
    
    list(pop = pop, popEval = popEval)
}

setMethod("replacement", "EMNAa", replacementEMNAa)


replacementEMNAi <- function (eda, currGen, pop, popEval, selectedPop,
        selectedEval, sampledPop, sampledEval) {
    list(pop = rbind(pop, sampledPop), popEval = c(popEval, sampledEval))
}

setMethod("replacement", "EMNAi", replacementEMNAi)


runEMNAai <- function (eda, f, lower, upper, trace = FALSE) {
    edaClass <- class(eda)
    shouldReset <- existsMethod("selection", edaClass)
    selectionReal <- selectMethod("selection", edaClass)
    selectionWrap <- function (eda, currGen, pop, popEval) {
        if (currGen == 1) {
            selectionReal(eda, currGen, pop, popEval)
        } else {
            seq(along = popEval)
        }
    }
    setMethod("selection", edaClass, selectionWrap)
    result <- callNextMethod()
    removeMethod("selection", edaClass)
    if (shouldReset) {
        setMethod("selection", edaClass, selectionReal)
    }
    
    result
}

setMethod("run", "EMNAa", runEMNAai)

setMethod("run", "EMNAi", runEMNAai)
