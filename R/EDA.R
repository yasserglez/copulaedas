# edas: R package for Estimation of Distribution Algorithms
# Copyright (C) 2010-2011 Yasser González-Fernández
# Copyright (C) 2010-2011 Marta Soto
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

setClass("EDA", 
    contains = "VIRTUAL",
    representation = representation(
        name = "character",
        parameters = "list"),
    prototype = prototype(
        name = "Estimation of Distribution Algorithm",
        parameters = list()))


setGeneric("seeding",
    function (eda, lower, upper) 
      standardGeneric("seeding"),
    signature = "eda",
    useAsDefault = FALSE)

setGeneric("selection",
    function (eda, currGen, pop, popEval) 
      standardGeneric("selection"),
    signature = "eda",
    useAsDefault = FALSE)

setGeneric("learning",
    function (eda, currGen, oldModel, selectedPop, selectedEval)
      standardGeneric("learning"),
    signature = "eda",
    useAsDefault = FALSE)

setGeneric("sampling",
    function (eda, currGen, model, lower, upper) 
      standardGeneric("sampling"),
    signature = "eda",
    useAsDefault = FALSE)

setGeneric("repairing",
    function (eda, currGen, pop, lower, upper) 
      standardGeneric("repairing"),
    signature = "eda",
    useAsDefault = FALSE)

setGeneric("replacement",
    function (eda, currGen, pop, popEval, 
        selectedPop, selectedEval, sampledPop, sampledEval)
      standardGeneric("replacement"),
    signature = "eda",
    useAsDefault = FALSE)

setGeneric("optimization",
    function (eda, currGen, pop, popEval, f, lower, upper) 
      standardGeneric("optimization"),
    signature = "eda", 
    useAsDefault = FALSE)

setGeneric("termination",
    function (eda, currGen, fEvals, pop, popEval)
      standardGeneric("termination"),
    signature = "eda",
    useAsDefault = FALSE)

setGeneric("reporting",
    function (eda, currGen, fEvals, model, pop, popEval, selectedPop,
        selectedEval, sampledPop, sampledEval)
      standardGeneric("reporting"),
    signature = "eda",
    useAsDefault = FALSE)

setGeneric("run",
    function (eda, f, lower, upper, trace = FALSE)
      standardGeneric("run"),
    signature = "eda",
    useAsDefault = FALSE)


showEDA <- function (object) {
  cat("Estimation of Distribution Algorithm\n\n")
  cat("Name:", object@name, "\n")
}

setMethod("show", "EDA", showEDA)
