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

runEDA <- function(eda, popSize, f, lower, upper) {
  fEvals <- 0 
  fWrap <- function(...) { fEvals <<- fEvals + 1; f(...) }
  evaluation <- function(eda, currGen, pop, fWrap, lower, upper) {
    pop <- repairing(eda, currGen, pop, lower, upper)
    popEval <- sapply(seq(nrow(pop)), function(i) fWrap(pop[i, ]))
    optimization(eda, currGen, pop, popEval, fWrap, lower, upper)
  }
  
  currGen <- 0
  
  pop <- seeding(eda, popSize, lower, upper)
  
  model <- NULL
  
  optimResult <- evaluation(eda, currGen, pop, fWrap, lower, upper)
  pop <- optimResult$pop
  popEval <- optimResult$popEval
  
  while(!termination(eda, currGen, fEvals, pop, popEval)) {
    selectedInd <- selection(eda, currGen, pop, popEval)
    selectedPop <- pop[selectedInd, ]
    selectedEval <- popEval[selectedInd]
    
    model <- learning(eda, currGen, model, selectedPop, selectedEval)

    sampledPop <- sampling(eda, currGen, model, popSize, lower, upper)

    optimResult <- evaluation(eda, currGen, sampledPop, fWrap, lower, upper)
    sampledPop <- optimResult$pop
    sampledEval <- optimResult$popEval
    
    reporting(eda, currGen, fEvals, pop, popEval, selectedPop,
        selectedEval, sampledPop, sampledEval)

    replaceResult <- replacement(eda, currGen, pop, popEval, selectedPop, 
        selectedEval, sampledPop, sampledEval)
    pop <- replaceResult$pop
    popEval <- replaceResult$popEval

  	currGen <- currGen + 1
  }
  list(bestEval = min(popEval), fEvals = fEvals, numGens = currGen)
}

setMethod("run", "EDA", runEDA)


runEMNAai <- function(eda, popSize, f, lower, upper) {
  edaClass <- class(eda)
  shouldReset <- existsMethod("selection", edaClass)
  selectionReal <- selectMethod("selection", edaClass)
  selectionWrap <- function(eda, currGen, pop, popEval) {
    if (currGen == 0) {
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
