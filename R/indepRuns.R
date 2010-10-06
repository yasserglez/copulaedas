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

indepRuns <- function (eda, f, lower, upper, runs = 3, verbose = FALSE, file = NULL) {
  if (!is.null(file)) sink(file)

  results <- list()
  for (run in seq(length = runs)) {
    result <- run(eda, f, lower, upper)
    results <- c(results, result)
    
    if (verbose) {
      if (run == 1) {
        w <- max(getOption("digits") + 5, 18)
        h <- c("Run", "Generations", "Evaluations", 
            "Best Evaluation", "Elapsed Time")
        cat(format(h, justify = "right", width = w), "\n")
      }
      cat(format(c(run, result@numGens, result@fEvals), width = w),
          format(result@bestEval, scientific = TRUE, width = w),
          format(result@totalTime[3], width = w),
          "\n")
    }
  }
  
  if (verbose && runs > 1) {
    numGens <- sapply(results, function (r) r@numGens)
    fEvals <- sapply(results, function (r) r@fEvals)
    bestEval <- sapply(results, function (r) r@bestEval)
    elapsedTime <- sapply(results, function (r) r@totalTime[3])
    
    cat("\n")
    h <- c("", "Generations", "Evaluations", "Best Evaluation", "Elapsed Time")
    cat(format(h, justify = "right", width = w), "\n")
    cat(format("Mean", justify = "right", width = w),
        format(c(mean(numGens), mean(fEvals)), width = w),
        format(mean(bestEval), scientific = TRUE, width = w),
        format(mean(elapsedTime), width = w),
        "\n")
    cat(format("Standard Deviation", justify = "right", width = w),
        format(c(sd(numGens), sd(fEvals)), width = w),
        format(sd(bestEval), scientific = TRUE, width = w),
        format(sd(elapsedTime), width = w),
        "\n")
  }

  if (!is.null(file)) sink(NULL)
  
  invisible(results)
}
