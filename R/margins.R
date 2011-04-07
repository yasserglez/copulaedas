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

# Normal (pnorm and qnorm defined in the stats package).

fnorm <- function (x, lower, upper) {
    list(mean = mean(x), sd = sd(x))
}

# Truncated Normal (ptruncnorm and qtruncnorm defined in the truncnorm package).

ftruncnorm <- function (x, lower, upper) {
    list(a = lower, b = upper, mean = mean(x), sd = sd(x))
}

# Kernel-smoothed empirical margins. The sample is transformed into Uniform
# variables using the Empirical CDF (modified to avoid problems in the boundary
# of the interval). The inverse of the CDF is computed using the Newton-Raphson
# method using the sample quantiles as initial values. See Azzalini, A. (1981)
# A note on the estimation of a distribution function and quantiles by a kernel
# method, Biometrika, 68, 326-328.

fempirical <- function (x, lower, upper) {
    list(X = x, b = bw.nrd0(x))
}

pempirical <- function (q, X, b) {
    rank(q) / (length(q) + 1)
}

qempirical <- function (p, X, b) {
    eps <- .Machine$double.eps^0.5
    quantiles <- quantile(X, p, names = FALSE)
    n <- length(X)
    f <- function (x) sum(dnorm((x - X) / b)) / (n * b)
    F <- function (x) sum(pnorm((x - X) / b)) / n
    sapply(seq(along = p), 
            function (i) {
                iter <- 0
                x <- quantiles[i]
                Fx <- F(x) - p[i]
                while (abs(Fx) > eps && iter <= 100) {
                    fx <- f(x)
                    if (is.finite(fx) && abs(fx) > eps) {
                        x <- x - Fx / fx
                        Fx <- F(x) - p[i]
                        iter <- iter + 1
                    } else {
                        break
                    }
                }
                x
            })
}
