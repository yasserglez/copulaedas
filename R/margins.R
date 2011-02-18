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

fspline <- function (x) {
    p <- rank(x) / length(x)
    p[which.min(p)] <- 0
    spline <- smooth.spline(p, x, keep.data = FALSE)
    list(spline = spline)
}

pspline <- function (q, spline) {
    rank(q) / (length(q) + 1)
}

qspline <- function (p, spline) {
    predict(spline, p)$y
}


fnorm <- function (x) {
    list(mean = mean(x), sd = sd(x))
}
