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

reportingDisabled <- function (eda, currGen, fEvals, model, pop, popEval, 
    selectedPop, selectedEval, sampledPop, sampledEval) {
}

setMethod("reporting", "EDA", reportingDisabled)


reportingSimple <- function (eda, currGen, fEvals, model, pop, popEval, 
    selectedPop, selectedEval, sampledPop, sampledEval) {
  w <- max(getOption("digits") + 5, 18)
  if (currGen == 1) {
    cat("\n")
    h <- c("Generation", "Minimum", "Mean", "Standard Deviation")
    cat(format(h, justify = "right", width = w), "\n")
  }
  stats <- c(min(sampledEval), mean(sampledEval), sd(sampledEval))
  cat(format(currGen, width = w),
      format(stats, scientific = TRUE, width = w), 
      "\n")
}
