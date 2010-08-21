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

reportingDisabled <- function(eda, currGen, fEvals, pop, popEval, selectedPop,
    selectedEval, sampledPop, sampledEval) {
}

setMethod("reporting", "EDA", reportingDisabled)


reportingSimple <- function(eda, currGen, fEvals, pop, popEval, selectedPop,
    selectedEval, sampledPop, sampledEval) {
  printRow <- function(parts, ...) {
    formated <- format(parts, width = 12, ...)
    cat(paste(formated, collapse = "    "), "\n")
  }
  if (currGen == 0) {
    printRow(list("Generation", "Minimum", "Mean", "Deviation"), 
        justify = "centre")
  }
  printRow(list(paste(currGen), min(popEval), mean(popEval), sd(popEval)),
      justify = "right", scientific = TRUE)
}
