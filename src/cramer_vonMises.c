/**
 * Multivariate empirical copula
 *
 * @param U pseudo-observations
 * @param n sample size
 * @param p dimension of the pseudo-observations
 * @param V is vector representing a matrix of dimension m x p
 * @param m "number of lines" of V
 * @param k "line" of V at which to compute the empirical copula
 * @param o offset (usually 0.0)
 * @return the value of the empirical copula at V[k + m * j], j=1...p
 * @author Ivan Kojadinovic
 */
double multCn(const double U[], int n, int p, const double V[], int m, int k, double o) {
    double sumind = 0.0;
    for (int i = 0; i < n; i++) {
	int ind = 1;
	for (int j = 0; j < p; j++)
	    ind *= (U[i + n * j] <= V[k + m * j]);
	sumind += (double)ind;
    }
    return sumind / (n + o);
}

/**
 * Cramer-von Mises test statistic
 *
 * @param n sample size
 * @param p dimension
 * @param U pseudo-observations
 * @param Ctheta values of the fitted copula at U
 * @param stat value of the test statistic
 * @author Ivan Kojadinovic
 */
void cramer_vonMises(int *n, int *p, double *U, double *Ctheta,
		     double *stat) {
  double s = 0.;
  for(int k = 0; k < *n; k++) {
    double diff = multCn(U, *n, *p, U, *n, k, 0.0) - Ctheta[k];
    s += diff * diff;
  }
  *stat = s;
}
