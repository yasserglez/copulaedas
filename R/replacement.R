# edas: R package for Estimation of Distribution Algorithms
# Copyright (C) 2010, 2011 Yasser González-Fernández
# Copyright (C) 2010, 2011 Marta Soto
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

replacementFull <- function (eda, currGen, pop, popEval, selectedPop,
        selectedEval, sampledPop, sampledEval) {
    list(pop = sampledPop, popEval = sampledEval)
}

setMethod("replacement", "EDA", replacementFull)
