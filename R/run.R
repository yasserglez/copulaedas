# edas: R package for Estimation of Distribution Algorithms
# Copyright (C) 2010 Yasser González-Fernández <ygonzalezfernandez@gmail.com>
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

setOldClass("proc_time")

setClass("EDAResult",
    representation = representation(
        eda = "EDA",
        f = "function",
        lower = "numeric",
        upper = "numeric",
        numGens = "numeric",
        fEvals = "numeric",
        bestEval = "numeric",
        bestIndiv = "numeric",
        totalTime = "proc_time"))

setClass("EDATracedResult",
    contains = "EDAResult",
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


showEDAResult <- function (object) {
  names <- c(
      "Best function evaluation",
      "Number of generations",
      "Number of function evaluations",
      "Total runtime (CPU time)",
      "Total runtime (Elapsed time)")
  values <- c(
      format(object@bestEval, scientific = TRUE),
      format(object@numGens),
      format(object@fEvals),
      paste(format(sum(object@totalTime, na.rm = TRUE) - object@totalTime[3]), "seconds"),
      paste(format(object@totalTime[3]), "seconds"))
  
  cat(object@eda@name, "Results\n\n")
  width <- max(nchar(names))
  for (i in seq(along = names)) {
    cat(format(names[i], width = width), values[i], "\n")
  }
}

setMethod("show", "EDAResult", showEDAResult)


runEDA <- function (eda, f, lower, upper, trace = FALSE) {
  # Initialize the statistics of the run.
  numGens <- 0
  fEvals <- 0; fWrap <- function (...) { fEvals <<- fEvals + 1; f(...) }
  startTime <- proc.time()
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
      if (trace) selectionTimes <- c(selectionTimes, list(proc.time() - proc.time()))
      model <- NULL
      if (trace) learningTimes <- c(learningTimes, list(proc.time() - proc.time()))
      if (trace) t0 <- proc.time()
      sampledPop <- seeding(eda, lower, upper)
      if (trace) samplingTimes <- c(samplingTimes, list(proc.time() - t0))
    } else {
      if (trace) t0 <- proc.time()
      selectedInd <- selection(eda, numGens, pop, popEval)
      selectedPop <- pop[selectedInd, ]
      selectedEval <- popEval[selectedInd]
      if (trace) selectionTimes <- c(selectionTimes, list(proc.time() - t0))
      if (trace) t0 <- proc.time()
      model <- learning(eda, numGens, model, selectedPop, selectedEval)
      if (trace) learningTimes <- c(learningTimes, list(proc.time() - t0))
      if (trace) t0 <- proc.time()
      sampledPop <- sampling(eda, numGens, model, lower, upper)
      if (trace) samplingTimes <- c(samplingTimes, list(proc.time() - t0))
    }

    # Evaluate the generated population.
    if (trace) t0 <- proc.time()
    sampledPop <- repairing(eda, numGens, sampledPop, lower, upper)
    if (trace) repairingTimes <- c(repairingTimes, list(proc.time() - t0))
    if (trace) t0 <- proc.time()
    sampledEval <- sapply(seq(length = nrow(sampledPop)), 
        function (i) fWrap(sampledPop[i, ]))
    if (trace) evaluationTimes <- c(evaluationTimes, list(proc.time() - t0))
    if (trace) t0 <- proc.time()
    optimResult <- optimization(eda, numGens, sampledPop, sampledEval, fWrap, lower, upper)	
    sampledPop <- optimResult$pop
    sampledEval <- optimResult$popEval
    if (trace) optimizationTimes <- c(optimizationTimes, list(proc.time() - t0))

    # Execute the function to report progress. 
    reporting(eda, numGens, fEvals, model, pop, popEval, selectedPop,
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
    if (trace) t0 <- proc.time()
    replaceResult <- replacement(eda, numGens, pop, popEval, selectedPop, 
        selectedEval, sampledPop, sampledEval)
    pop <- replaceResult$pop
    popEval <- replaceResult$popEval
    if (trace) replacementTimes <- c(replacementTimes, list(proc.time() - t0))
    
    # Evaluate the termination conditions.
    if (termination(eda, numGens, fEvals, pop, popEval)) break
  }
  
  result <- new("EDAResult",
      eda = eda,
      f = f,
      lower = lower,
      upper = upper, 
      numGens = numGens,
      fEvals = fEvals,
      bestEval = bestEval,
      bestIndiv = bestIndiv,
      totalTime = proc.time() - startTime)
  
  if (trace) {
    result <- new("EDATracedResult", result,
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
