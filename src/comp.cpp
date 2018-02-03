#include "Rcpp.h"

// [[Rcpp::depends(Rcpp)]]
using namespace Rcpp;

double factorial(int x) {
	double res = 1.0;
	for (double d = 1.0; d <= x; ++d) {
		res *= d;
	}
	return res;
}

double W(double lam, double nu, int sumTo) {
	double sum = 0.0;
	double factorial = 1.0;
	double lamPower = lam;
	for (int i = 1; i <= sumTo; ++i) {
		factorial *= i;
		sum += lamPower * log(factorial) / pow(factorial, nu);
		lamPower *= lam;
	}
	return sum;
}

double Y(double lam, double nu, int sumTo) {
	double sum = 0.0;
	double factorial = 1.0;
	double lamPower = lam;
	for (int i = 1; i <= sumTo; ++i) {
		factorial *= i;
		sum += lamPower * i / pow(factorial, nu);
		lamPower *= lam;
	}
	return sum;
}

double Z(double lam, double nu, int sumTo) {
	double sum = 1.0;
	double factorial = 1.0;
	double lamPower = lam;
	for (int i = 1; i <= sumTo; ++i) {
		factorial *= i;
		sum += lamPower / pow(factorial, nu);
		lamPower *= lam;
	}
	return sum;
}

// [[Rcpp::export]]
NumericVector W(NumericVector lam, NumericVector nu, int sumTo) {
	int size = lam.size();
	NumericVector out(size);
	for (int i = 0; i < size; ++i) {
		out[i] = W(lam[i], nu[i], sumTo);
	}
	return out;
}

// [[Rcpp::export]]
NumericVector Y(NumericVector lam, NumericVector nu, int sumTo) {
	int size = lam.size();
	NumericVector out(size);
	for (int i = 0; i < size; ++i) {
		out[i] = Y(lam[i], nu[i], sumTo);
	}
	return out;
}

// [[Rcpp::export]]
NumericVector Z(NumericVector lam, NumericVector nu, int sumTo) {
	int size = lam.size();
	NumericVector out(size);
	for (int i = 0; i < size; ++i) {
		out[i] = Z(lam[i], nu[i], sumTo);
	}
	return out;
}

double dcomp(int y, double lam, double nu, int sumTo) {
	if (y < 0) {
		return 0.0;
	} else {
		return pow(lam, y) / (pow(factorial(y), nu) * Z(lam, nu, sumTo));
	}
}

void checkInputs(NumericVector lam, NumericVector nu) {

	int lamSize = lam.size();
	int nuSize = nu.size();

	for (int i = 0; i < lamSize; ++i) {
		if (lam[i] < 0) {
			throw exception("input 'lam' should be >= 0");
		}
	}

	for (int i = 0; i < nuSize; ++i) {
		if (nu[i] < 0) {
			throw exception("input 'nu' should be >= 0");
		}
	}
}

//' @title Conway-Maxwell-Poisson Probability Density Function
//' @author Jeffrey Pollock <jeffpollock9@@gmail.com>
//'
//' @description The PDF of the Conway-Maxwell-Poisson distribution with
//' parameters \code{lam} and \code{nu} at point \code{y}.
//'
//' @details The Conway-Maxwell-Poisson distribution has density:
//' \deqn{P(y) = \frac{\lambda^y}{(y!)^\nu Z(\lambda, \nu)}}{%
//' 	  P(y) = \lambda^y / ((y!)^\nu Z(\lambda, \nu))}
//'       for \eqn{y = 0, 1, 2, \ldots},  and:
//' \deqn{Z(\lambda, \nu) = \sum_{j=0}^\infty \frac{\lambda^j}{(j!)^\nu}}{%
//'       Z(\lambda, \nu) = \sum \lambda^j / ((j!)^\nu)}
//' where the summation is approximated by summing from \code{j = 0} to \code{sumTo}.
//'
//' @param y an integer vector where the density is to be calculated. If the input is not an
//' integer, it will be coerced to be an integer.
//' @param lam a double vector of the parameter \eqn{\lambda}.
//' @param nu a double vector of the parameter \eqn{\nu}.
//' @param sumTo an integer for the summation term in the density (default 100).
//' @param logP a boolean for if the log of the density should be given (default \code{FALSE}).
//'
//' @return The value of the PDF (or log PDF if \code{logP = TRUE}). Input vectors
//' are recycled to be same length.
//'
//' @examples
//' dcomp(-5:5, 2.5, 1)
//' dcomp(2, 2.5, 1.5)
//' require(graphics)
//' require(stats)
//' comp <- dcomp(0:10, 2.5, 0.9)
//' poisson <- dpois(0:10, 2.5)
//' barplot(rbind(comp, poisson), beside = TRUE, names= 0:10, legend.text = TRUE)
//' @export
// [[Rcpp::export]]
NumericVector dcomp(IntegerVector y, NumericVector lam, NumericVector nu,
		int sumTo = 100, bool logP = false) {

	checkInputs(lam, nu);

	int size = max(NumericVector::create(y.size(), lam.size(), nu.size()));

	y = rep_len(y, size);
	lam = rep_len(lam, size);
	nu = rep_len(nu, size);

	NumericVector ans = NumericVector(size);

	for (int i = 0; i < size; ++i) {
		ans[i] = dcomp(y[i], lam[i], nu[i], sumTo);
	}

	if (logP) {
		return log(ans);
	} else {
		return ans;
	}
}

//' @title Conway-Maxwell-Poisson Cumulative Density Function
//' @author Jeffrey Pollock <jeffpollock9@@gmail.com>
//'
//' @description The CDF of the Conway-Maxwell-Poisson distribution with parameters
//' \code{lam} and \code{nu} at point \code{q}.
//'
//' @details See \code{\link{dcomp}} for details of the PDF.
//'
//' @param q an integer vector where the CDF is to be calculated. If the input is not an
//' integer, it will be coerced to be an integer.
//' @param lam a double vector of the parameter \eqn{\lambda}.
//' @param nu a double vector of the parameter \eqn{\nu}.
//' @param sumTo an integer for the summation term in the density (default 100)
//' @param lowerTail a boolean for if P(Y<=q) should be returned
//' (default \code{TRUE}), otherwise, P(Y>q) is returned.
//' @param logP a boolean for if the log of the probability should be given (default \code{FALSE})
//'
//' @return The value of the CDF (or 1-CDF if \code{lowerTail = FALSE}) or log of this value if
//' \code{logP = TRUE}. Input vectors are recycled to be same length.
//'
//' @examples
//' pcomp(1:10, 1.5, 1.2)
//' @export
// [[Rcpp::export]]
NumericVector pcomp(IntegerVector q, NumericVector lam, NumericVector nu,
		int sumTo = 100, bool lowerTail = true, bool logP = false) {

	checkInputs(lam, nu);

	int size = max(NumericVector::create(q.size(), lam.size(), nu.size()));

	q = rep_len(q, size);
	lam = rep_len(lam, size);
	nu = rep_len(nu, size);

	NumericVector ans = NumericVector(size);

	for (int i = 0; i < size; ++i) {
		for (int j = 0; j <= q[i]; ++j) {
			ans[i] += dcomp(j, lam[i], nu[i], sumTo);
		}
	}

	if (!lowerTail) {
		ans = 1.0 - ans;
	}

	if (logP) {
		return log(ans);
	} else {
		return ans;
	}
}

//' @title Conway-Maxwell-Poisson Random Sample
//' @author Jeffrey Pollock <jeffpollock9@@gmail.com>
//'
//' @description Provides a size n sample from the Conway-Maxwell-Poisson distribution
//' with parameters \code{lam} and \code{nu}. Sampling is done via a simple multinomial approach.
//'
//' @details The function is only implemented for single values of \code{lam} and \code{nu}.
//' See \code{\link{dcomp}} for details of the PDF.
//'
//' @param n an integer of the number of random samples to be taken.
//' @param lam a double of the parameter \eqn{\lambda}.
//' @param nu a double of the parameter \eqn{\nu}.
//' @param sumTo an integer for the summation term in the density (default 100).
//'
//' @return A random sample of size n.
//'
//' @examples
//' require(graphics)
//' sample <- rcomp(1000, 8.5, 0.9)
//' barplot(table(sample))
//' @export
// [[Rcpp::export]]
IntegerVector rcomp(int n, double lam, double nu, int sumTo = 100) {
	NumericVector dist(sumTo);
	for (int i = 0; i < sumTo; ++i) {
		dist[i] = dcomp(i, lam, nu, sumTo);
	}
	IntegerVector sample(n);
	double u, sum;
	for (int i = 0; i < n; ++i) {
		u = R::runif(0.0, 1.0);
		sum = 0.0;
		for (int j = 0; j < sumTo; ++j) {
			sum += dist[j];
			if (u < sum) {
				sample[i] = j;
				break;
			}
		}
	}
	return sample;
}
