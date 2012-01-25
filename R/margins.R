# copulaedas: Estimation of Distribution Algorithms Based on Copulas
# Copyright (C) 2010-2012 Yasser González-Fernández <ygf@icimaf.cu>
# Copyright (C) 2010-2012 Marta Soto <mrosa@icimaf.cu>
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

# Kernel-smoothed empirical margins. The sample is transformed into uniform
# variables using the empirical c.d.f (modified to avoid problems in the boundary
# of the interval). The inverse of the c.d.f is computed from the kernel-smoothed
# c.d.f using the Newton-Raphson method with the sample quantiles as initial values. 
# See Azzalini A (1981). "A Note on the Estimation of a Distribution Function 
# and Quantiles by a Kernel Method." Biometrika, 68(1), 326-328.


fkernel <- function (x, lower, upper) {
    list(X = x, h = bw.nrd0(x))
}

pkernel <- function (q, X, h) {
    # Empirical c.d.f.
    rank(q) / (length(q) + 1)
}

qkernel <- function (p, X, h) {
    eps <- .Machine$double.eps^0.5
    maxIter <- 100
    quantiles <- quantile(X, p, names = FALSE)
    n <- length(X)
    f <- function (x) sum(dnorm((x - X) / h)) / (n * h)
    F <- function (x) sum(pnorm((x - X) / h)) / n
    sapply(seq(along = p),
        function (i) {
            iter <- 0
            x <- quantiles[i]
            Fx <- F(x) - p[i]
            while (abs(Fx) > eps && iter <= maxIter) {
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
