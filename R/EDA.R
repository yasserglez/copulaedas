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


setGeneric("seeding",
    signature = "eda",
    def = function(eda, popSize, lower, upper) 
            standardGeneric("seeding"))

setGeneric("selection",
    signature = "eda",
    def = function(eda, currGen, pop, popEval)
            standardGeneric("selection"))

setGeneric("learning",
    signature = "eda",
    def = function(eda, currGen, oldModel, selectedPop, selectedEval)
            standardGeneric("learning"))

setGeneric("sampling",
    signature = "eda",
    def = function(eda, currGen, model, popSize, lower, upper)
            standardGeneric("sampling"))

setGeneric("repairing",
    signature = "eda",
    def = function(eda, currGen, pop, lower, upper)
            standardGeneric("repairing"))

setGeneric("replacement",
    signature = "eda",
    def = function(eda, currGen, pop, popEval, selectedPop, 
              selectedEval, sampledPop, sampledEval) 
            standardGeneric("replacement"))

setGeneric("optimization",
    signature = "eda",
    def = function(eda, currGen, pop, popEval, f, lower, upper) 
            standardGeneric("optimization"))

setGeneric("termination",
    signature = "eda",
    def = function(eda, currGen, fEvals, pop, popEval) 
            standardGeneric("termination"))

setGeneric("reporting",
    signature = "eda",
    def = function(eda, currGen, fEvals, pop, popEval, selectedPop,
              selectedEval, sampledPop, sampledEval)
            standardGeneric("reporting"))

setGeneric("run",
    signature = "eda",
    def = function(eda, popSize, f, lower, upper, ...)
            standardGeneric("run"))
