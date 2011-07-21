# copulaedas: Estimation of Distribution Algorithms based on Copula Theory
# Copyright (C) 2010, 2011 Yasser González-Fernández <ygf@icimaf.cu>
# Copyright (C) 2010, 2011 Marta Soto <mrosa@icimaf.cu>
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


VEDA <- function (...) {
    new("VEDA", parameters = list(...))
}


edaLearnVEDA <- function (eda, gen, previousModel,
        selectedPop, selectedEval, lower, upper) {
    fmargin <- eda@parameters$fmargin
    pmargin <- eda@parameters$pmargin
    vine <- eda@parameters$vine
    copulas <- eda@parameters$copulas
    indepTestSigLevel <- eda@parameters$indepTestSigLevel

    if (is.null(fmargin)) fmargin <- fempirical
    if (is.null(pmargin)) pmargin <- pempirical
    if (is.null(vine)) vine <- "DVine"
    if (is.null(copulas)) copulas <- c("normal")
    if (is.null(indepTestSigLevel)) indepTestSigLevel <- 0.01

    # Select the bivariate copula for each arc in the vine.
    selectCopula <- function (vine, j, i, x, y) {
        data <- cbind(x, y)

        # Hypothesis test for independence.
        pvalue <- indepTest(data, indepTestStat)$global.statistic.pvalue
        if (pvalue > indepTestSigLevel) {
            return(indepCopula())
        }

        # If the Independence copula was not selected, fit each candidate
        # copula and select the one that better fits the data.
        selectedCopula <- NULL
        selectedStat <- Inf
        tau <- cor(x, y, method = "kendall")
        for (copula in copulas) {
            if (identical(copula, "normal")) {
                candidateCopula <- fitCopula(copula = normalCopula(0),
                        data = data, method = "itau",
                        estimate.variance = FALSE)@copula
            } else if (identical(copula, "t")) {
                rho <- calibKendallsTau(normalCopula(0), tau)
                L <- function (df) loglikCopula(c(rho, df), data, normalCopula(0))
                df <- optimize(L, c(1, 30), maximum = TRUE)$maximum
                candidateCopula <- tCopula(rho, df = df, df.fixed = TRUE)
            } else if (identical(copula, "clayton")) {
                # Setting bounds to Kendall's tau to avoid numerical
                # problems evaluating the density.
                theta <- calibKendallsTau(claytonCopula(1), max(0, min(tau, 0.95)))
                candidateCopula <- claytonCopula(theta)
            } else if (identical(copula, "gumbel")) {
                # Setting bounds to Kendall's tau to avoid numerical
                # problems evaluating the density.
                theta <- calibKendallsTau(gumbelCopula(1), max(0, min(tau, 0.95)))
                candidateCopula <- gumbelCopula(theta)
            } else {
                stop("copula ", dQuote(copula), " not supported")
            }

            # Compute the value of the goodness-of-fit test statistic.
            candidateStat <- .C("cramer_vonMises",
                    as.integer(nrow(data)),
                    as.integer(ncol(data)),
                    as.double(data),
                    as.double(pcopula(candidateCopula, data)),
                    stat = double(1.0),
                    PACKAGE = "copula")$stat

            # Select the copula with the smaller value of the statistic.
            if (candidateStat < selectedStat) {
                selectedStat <- candidateStat
                selectedCopula <- candidateCopula
            }
        }

        selectedCopula
    }

    n <- ncol(selectedPop)
    m <- nrow(selectedPop)

    # Fit marginal distributions and transform to uniform variables.
    margins <- lapply(seq(length = n),
            function (i) fmargin(selectedPop[ , i], lower[i], upper[i]))
    uniformPop <- sapply(seq(length = n),
            function (i) do.call(pmargin,
                        c(list(selectedPop[ , i]), margins[[i]])))

    # Select an ordering for the variables and reorder the dataset.
    colnames(uniformPop) <- if (is.null(colnames(selectedPop)))
                as.character(seq(length = n)) else 
                colnames(selectedPop)
    ordering <- vineOrder(type = vine, data = uniformPop,
            method = "greedy", according = "kendall")
    orderedPop <- uniformPop[ , ordering]

    # Simulate the distribution of the test statistic for the independence
    # test based on the CvM statistic. The distribution is simulated once
    # and used for the rest of the generations.

    indepTestStat <- previousModel$indepTestStat
    if (is.null(indepTestStat)) {
        indepTestStat <- indepTestSim(m, 2, print.every = -1)
    }

    vine <- do.call(vineFit,
            list(type = vine, data = orderedPop,
                 selectCopula = selectCopula,
                 truncMethod = "AIC",
                 method = "ml", optimMethod = ""))@vine

    list(vine = vine, margins = margins, ordering = ordering,
            indepTestStat = indepTestStat)
}

setMethod("edaLearn", "VEDA", edaLearnVEDA)


edaSampleVEDA <- function (eda, gen, model, lower, upper) {
    popSize <- eda@parameters$popSize
    qmargin <- eda@parameters$qmargin

    if (is.null(popSize)) popSize <- 100
    if (is.null(qmargin)) qmargin <- qempirical

    orderedPop <- rvine(model$vine, popSize)

    uniformPop <- matrix(NA, nrow(orderedPop), ncol(orderedPop))
    for (k in seq(length = ncol(orderedPop))) {
        uniformPop[ , model$ordering[k]] <- orderedPop[ , k]
    }

    finalPop <- sapply(seq(length = ncol(uniformPop)),
            function (i) do.call(qmargin,
                    c(list(uniformPop[ , i]), model$margins[[i]])))
    finalPop <- matrix(finalPop, nrow = popSize)

    # Keeping the names of the variables.
    if (!is.null(dimnames(model$vine))) {
        vineDimnames <- dimnames(model$vine)
        finalPopNames <- character(ncol(orderedPop))
        for (k in seq(length = ncol(orderedPop))) {
            finalPopNames[model$ordering[k]] <- vineDimnames[k]
        }    
        colnames(finalPop) <- finalPopNames
    }

    finalPop
}

setMethod("edaSample", "VEDA", edaSampleVEDA)
