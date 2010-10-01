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

setClass("EDA", 
    contains = "VIRTUAL",
    representation = representation(
        name = "character",
        seedingArgs = "list",
        selectionArgs = "list",
        learningArgs = "list",
        samplingArgs = "list",
        repairingArgs = "list",
        replacementArgs = "list",
        optimizationArgs = "list",
        terminationArgs = "list",
        reportingArgs = "list"),
    prototype = prototype(
        name = "Estimation of Distribution Algorithm",
        seedingArgs = list(),
        selectionArgs = list(),
        learningArgs = list(),
        samplingArgs = list(),
        repairingArgs = list(),
        replacementArgs = list(),
        optimizationArgs = list(),
        terminationArgs = list(),
        reportingArgs = list()))


# Functions implementing the parts of the EDAs.

setGeneric("seeding",
    function (eda, popSize, lower, upper)
      standardGeneric("seeding"),
    signature = "eda")

setGeneric("selection",
    function (eda, currGen, pop, popEval)
      standardGeneric("selection"),
    signature = "eda")

setGeneric("learning",
    function (eda, currGen, oldModel, selectedPop, selectedEval)
      standardGeneric("learning"),
    signature = "eda")

setGeneric("sampling",
    function (eda, currGen, model, popSize, lower, upper)
      standardGeneric("sampling"),
    signature = "eda")

setGeneric("repairing",
    function (eda, currGen, pop, lower, upper)
      standardGeneric("repairing"),
    signature = "eda")

setGeneric("replacement",
    function (eda, currGen, pop, popEval, 
        selectedPop, selectedEval, sampledPop, sampledEval)
      standardGeneric("replacement"),
    signature = "eda")

setGeneric("optimization",
    function (eda, currGen, pop, popEval, f, lower, upper)
      standardGeneric("optimization"),
    signature = "eda")

setGeneric("termination",
    function (eda, currGen, fEvals, pop, popEval)
      standardGeneric("termination"),
    signature = "eda")

setGeneric("reporting",
    function (eda, currGen, fEvals, pop, popEval, selectedPop,
        selectedEval, sampledPop, sampledEval)
      standardGeneric("reporting"),
    signature = "eda")


# General functions on EDAs.

setGeneric("run",
    function (eda, popSize, f, lower, upper, trace = FALSE)
      standardGeneric("run"),
    signature = "eda")
