# edas: R package for Estimation of Distribution Algorithms
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

setClass("EDAResults", 
    contains = "list")


summaryEDAResults <- function (object) {
  numGens <- sapply(object, function (r) r@numGens)
  fEvals <- sapply(object, function (r) r@fEvals)
  bestEval <- sapply(object, function (r) r@bestEval)
  cpuTime <- sapply(object,
      function (r) sum(r@totalTime, na.rm = TRUE) - r@totalTime[3])
  elapsedTime <- sapply(object, function (r) r@totalTime[3])

  f <- function (x) c(min(x), median(x), max(x), mean(x), sd(x)) 
  data <- cbind(f(numGens), f(fEvals), f(bestEval), f(cpuTime), f(elapsedTime))
  
  colnames(data) <- c("Generations", "Evaluations", "Best Evaluation", 
      "CPU Time", "Elapsed Time")
  rownames(data) <- c("Minimum", "Median", "Maximum", "Mean", "Std. Dev.")

  data
}

setMethod("summary", "EDAResults", summaryEDAResults)


showEDAResults <- function (object) {
  numGens <- sapply(object, function (r) r@numGens)
  fEvals <- sapply(object, function (r) r@fEvals)
  bestEval <- sapply(object, function (r) r@bestEval)
  cpuTime <- sapply(object, 
      function (r) sum(r@totalTime, na.rm = TRUE) - r@totalTime[3])
  elapsedTime <- sapply(object, function (r) r@totalTime[3])

  data <- cbind(numGens, fEvals, bestEval, cpuTime, elapsedTime)
  colnames(data) <- c("Generations", "Evaluations", 
      "Best Evaluation", "CPU Time", "Elapsed Time")
  rownames(data) <- paste("Run", as.character(seq(length = nrow(data))))

  cat("\n")
  print(data)
  cat("\n")
}

setMethod("show", "EDAResults", showEDAResults)


indepRuns <- function (eda, f, lower, upper, trace = FALSE, runs = 3, verbose = FALSE) {

  results <- new("EDAResults")
  
  for (run in seq(length = runs)) {
    result <- run(eda, f, lower, upper)
    results <- as(c(results, result), "EDAResults")

    if (verbose) {
      if (run == 1) {
        cat("\n")
        w <- max(getOption("digits") + 5, 15)
        h <- c("Run", "Generations", "Evaluations", "Best Evaluation", 
            "CPU Time", "Elapsed Time")
        cat(format(h, justify = "right", width = w), "\n")
      }
      cat(format(c(run, result@numGens, result@fEvals), width = w),
          format(result@bestEval, scientific = TRUE, width = w),
          format(c(sum(result@totalTime, na.rm = TRUE) - result@totalTime[3],
                  result@totalTime[3]), width = w),
          "\n")
    }
  }

  if (verbose && runs > 1) {
    numGens <- sapply(results, function (r) r@numGens)
    fEvals <- sapply(results, function (r) r@fEvals)
    bestEval <- sapply(results, function (r) r@bestEval)
    cpuTime <- sapply(results, 
        function (r) sum(r@totalTime, na.rm = TRUE) - r@totalTime[3])
    elapsedTime <- sapply(results, function (r) r@totalTime[3])

    cat("\n")
    h <- c("", "Generations", "Evaluations", "Best Evaluation", 
        "CPU Time", "Elapsed Time")
    cat(format(h, justify = "right", width = w), "\n")
    fs <- list(min, median, max, mean, sd)
    rNames <- c("Minimum", "Median", "Maximum", "Mean", "Std. Dev.")
    for (i in seq(along = fs)) {
      f <- fs[[i]]
      cat(format(rNames[i], justify = "right", width = w),
          format(c(f(numGens), f(fEvals)), width = w),
          format(f(bestEval), scientific = TRUE, width = w),
          format(c(f(cpuTime), f(elapsedTime)), width = w),
          "\n")
    }
    cat("\n")
  }

  results <- as(results, "EDAResults")
  
  if (verbose) {
    invisible(results) 
  } else {
    results
  }
}
