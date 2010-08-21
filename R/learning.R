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

learningError <- function(eda, currGen, oldModel, selectedPop, selectedEval) {
  stop("learning method not specified")
}

setMethod("learning", "EDA", learningError)


learningEMNA <- function(eda, currGen, oldModel, selectedPop, selectedEval) {
  list(mu = colMeans(selectedPop), sigma = cov(selectedPop))
}

setMethod("learning", "EMNAg", learningEMNA)
setMethod("learning", "EMNAa", learningEMNA)
setMethod("learning", "EMNAi", learningEMNA)
