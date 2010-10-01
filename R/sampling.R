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

samplingError <- function (eda, currGen, model, popSize, lower, upper) {
  stop("sampling method not specified")
}

setMethod("sampling", "EDA", samplingError)


samplingEMNAg <- function (eda, currGen, model, popSize, lower, upper) {
  rmvnorm(popSize, model$mu, model$sigma)
}

setMethod("sampling", "EMNAg", samplingEMNAg)


samplingEMNAai <- function (eda, currGen, model, popSize, lower, upper) {
  rmvnorm(1, model$mu, model$sigma)
}

setMethod("sampling", "EMNAa", samplingEMNAai)
setMethod("sampling", "EMNAi", samplingEMNAai)
