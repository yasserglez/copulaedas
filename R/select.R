# copulaedas: Estimation of Distribution Algorithms based on Copulas
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

edaSelectTruncation <- function (eda, gen, pop, popEval) {
    truncFactor <- eda@parameters$truncFactor

    if (is.null(truncFactor)) truncFactor <- 0.3

    popOrder <- order(popEval)
    popOrder[seq(ceiling(truncFactor * length(popOrder)))]
}

setMethod("edaSelect", "EDA", edaSelectTruncation)
