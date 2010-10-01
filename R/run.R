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

setOldClass("difftime")

setClass("EDARun",
    representation = representation(
        eda = "EDA",
        popSize = "numeric",
        f = "function",
        lower = "numeric",
        upper = "numeric",
        numGens = "numeric",
        fEvals = "numeric",
        bestEval = "numeric",
        bestIndiv = "numeric",
        totalTime = "difftime"))

setClass("TracedEDARun",
    contains = "EDARun",
    representation = representation(
        models = "list",
        pops = "list",
        selectedPops = "list",
        sampledPops = "list",
        selectionTimes = "list",
        learningTimes = "list",
        samplingTimes = "list",
        repairingTimes = "list",
        evaluationTimes = "list",
        optimizationTimes = "list",
        replacementTimes = "list"))


runEDA <- function (eda, popSize, f, lower, upper, trace) {
  # Initialize global statistics of the run.
  numGens <- 0
  fEvals <- 0; fWrap <- function (...) { fEvals <<- fEvals + 1; f(...) }
  totalTime <- Sys.time()
  bestEval <- Inf
  bestIndiv <- NULL
  if (trace) {
    models <- list()
    pops <- list()
    selectedPops <- list()
    sampledPops <- list()
    selectionTimes = list()
    learningTimes = list()
    samplingTimes = list()
    repairingTimes = list()
    evaluationTimes = list()
    optimizationTimes = list()
    replacementTimes = list()
  }

  # Main loop of the EDA.
  repeat {
    numGens <- numGens + 1

    # Generating the new population.
    if (numGens == 1) {
      pop <- popEval <- NULL
      selectedPop <- selectedEval <- NULL
      if (trace) selectionTimes <- c(selectionTimes, list(as.difftime(0, units = "secs")))
      model <- NULL
      if (trace) learningTimes <- c(learningTimes, list(as.difftime(0, units = "secs")))
      if (trace) t0 <- Sys.time()
      sampledPop <- seeding(eda, popSize, lower, upper)
      if (trace) samplingTimes <- c(samplingTimes, list(Sys.time() - t0))
    } else {
      if (trace) t0 <- Sys.time()
      selectedInd <- selection(eda, numGens, pop, popEval)
      selectedPop <- pop[selectedInd, ]
      selectedEval <- popEval[selectedInd]
      if (trace) selectionTimes <- c(selectionTimes, list(Sys.time() - t0))
      if (trace) t0 <- Sys.time()
      model <- learning(eda, numGens, model, selectedPop, selectedEval)
      if (trace) learningTimes <- c(learningTimes, list(Sys.time() - t0))
      if (trace) t0 <- Sys.time()
      sampledPop <- sampling(eda, numGens, model, popSize, lower, upper)
      if (trace) samplingTimes <- c(samplingTimes, list(Sys.time() - t0))
    }
    
    # Evaluate the generated population.
    if (trace) t0 <- Sys.time()
    sampledPop <- repairing(eda, numGens, sampledPop, lower, upper)
    if (trace) repairingTimes <- c(repairingTimes, list(Sys.time() - t0))
    if (trace) t0 <- Sys.time()
    sampledEval <- sapply(seq(length = nrow(sampledPop)), 
        function (i) fWrap(sampledPop[i, ]))
    if (trace) evaluationTimes <- c(evaluationTimes, list(Sys.time() - t0))
    if (trace) t0 <- Sys.time()
    optimResult <- optimization(eda, numGens, sampledPop, 
        sampledEval, fWrap, lower, upper)	
    sampledPop <- optimResult$pop
    sampledEval <- optimResult$popEval
    if (trace) optimizationTimes <- c(optimizationTimes, list(Sys.time() - t0))
    
    # Execute the function to report progress. 
    reporting(eda, numGens, fEvals, pop, popEval, selectedPop,
        selectedEval, sampledPop, sampledEval)
    
    # Update global statistics of the run.
    if (min(sampledEval) < bestEval) {
      i <- which.min(sampledEval)
      bestEval <- sampledEval[i]
      bestIndiv <- sampledPop[i, ]
    }
    if (trace) {
      models <- c(models, list(model))
      pops <- c(pops, list(pop))
      selectedPops <- c(selectedPops, list(selectedPop))
      sampledPops <- c(sampledPops, list(sampledPop))
    }
    
    # Execute the replacement method.
    if (trace) t0 <- Sys.time()
    replaceResult <- replacement(eda, numGens, pop, popEval, selectedPop, 
        selectedEval, sampledPop, sampledEval)
    pop <- replaceResult$pop
    popEval <- replaceResult$popEval
    if (trace) replacementTimes <- c(replacementTimes, list(Sys.time() - t0))
    
    # Evaluate the termination conditions.
    if (termination(eda, numGens, fEvals, pop, popEval)) break
  }
  
  # Update global statistics of the run.
  totalTime <- Sys.time() - totalTime
  
  result <- new("EDARun",
      eda = eda,
      popSize = popSize, 
      f = f,
      lower = lower, 
      upper = upper, 
      numGens = numGens,
      fEvals = fEvals,
      bestEval = bestEval,
      bestIndiv = bestIndiv,
      totalTime = totalTime)
  
  if (trace) {
    result <- new("TracedEDARun", result,
        models = models,
        pops = pops,
        selectedPops = selectedPops,
        sampledPops = sampledPops,
        selectionTimes = selectionTimes,
        learningTimes = learningTimes,
        samplingTimes = samplingTimes,
        repairingTimes = repairingTimes,
        evaluationTimes = evaluationTimes,
        optimizationTimes = optimizationTimes,
        replacementTimes = replacementTimes)
  }
  
  result
}

setMethod("run", "EDA", runEDA)


runEMNAai <- function (eda, popSize, f, lower, upper, trace) {
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
