# edas: GNU R package for Estimation of Distribution Algorithms
# Copyright (C) 2010 Yasser González Fernández <yasserglez@gmail.com>
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

terminationMaxGen <- function(eda, currGen, fEvals, pop, popEval) {
  maxGen <- eda@terminationArgs$maxGen
  if (is.null(maxGen)) maxGen <- 100
  currGen >= maxGen
}

setMethod("termination", "EDA", terminationMaxGen)


terminationMaxEvals <- function(eda, currGen, fEvals, pop, popEval) {
  maxEvals <- eda@terminationArgs$maxEvals
  if (is.null(maxEvals)) maxEvals <- 10000
  fEvals >= maxEvals
}


terminationEval <- function(eda, currGen, fEvals, pop, popEval) {
  fEval <- eda@terminationArgs$fEval
  fEvalTol <- eda@terminationArgs$fEvalTol
  if (is.null(fEval)) fEval <- 0 
  if (is.null(fEvalTol)) fEvalTol <- 1e-08
  any(abs(popEval - fEval) <= fEvalTol)
}


terminationCombined <- function(..., requireAll = FALSE) {
  function(eda, currGen, fEvals, pop, popEval) {
    methods <- list(...)
    args <- list(eda, currGen, fEvals, pop, popEval)
    results <- sapply(methods, function(method) do.call(method, args))
    if (requireAll) all(results) else any(results)
  }
}
