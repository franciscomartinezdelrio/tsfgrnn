#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
List regression_2(double sigma, NumericMatrix patterns, NumericMatrix targets,
                  NumericVector example) {
  NumericVector w(patterns.nrow());
  for (int i = 0; i < w.size(); ++i)
    w[i] = std::exp(-1 / (2 * pow(sigma, 2)) * sum(pow(patterns(i, _) - example, 2)));
  double s = sum(w);
  if (s == 0) {
    for (int i = 0; i < w.size(); ++i)
      w[i] += 1.0 / w.size();
  } else {
    for (int i = 0; i < w.size(); ++i)
      w[i] /= s;
  }
  NumericVector prediction(targets.ncol());
  for (int col = 0; col < targets.ncol(); ++col) {
    for (int row = 0; row < targets.nrow(); ++row)
      prediction[col] += w[row] * targets(row, col);
  }
  List ret;
  ret["prediction"] = prediction;
  ret["weights"] = w;
  return ret;
}

// [[Rcpp::export]]
List build_examples2(NumericVector timeS, NumericVector lags, int nt) {
  const int MAXLAG = lags[0];
  const int NCOL   = lags.size();
  const int NROW   = timeS.size() - MAXLAG - nt + 1;
  NumericMatrix patterns(NROW, NCOL);
  NumericMatrix targets(NROW, nt);
  IntegerVector targetsI(NROW);
  int row = 0;
  for (int ind = MAXLAG + nt -1; ind < timeS.size(); ++ind) {
    for (int col = 0; col < NCOL; ++col)
      patterns(row, col) = timeS[ind - nt + 1 - lags[col]];
    targets(row, _) = timeS[Range(ind - nt + 1, ind + 1)];
    row++;
  }
  List ret;
  ret["patterns"] = patterns;
  ret["targets"]  = targets;
  return ret;
}

// [[Rcpp::export]]
List build_examples_m(NumericVector timeS, NumericVector lags, int nt) {
  const int MAXLAG = lags[0];
  const int NCOL   = lags.size();
  const int NROW   = timeS.size() - MAXLAG - nt + 1;
  NumericMatrix patterns(NROW, NCOL);
  NumericMatrix targets(NROW, nt);
  IntegerVector targetsI(NROW);
  int row = 0;
  for (int ind = MAXLAG + nt -1; ind < timeS.size(); ++ind) {
    double sum = 0;
    for (int col = 0; col < NCOL; ++col) {
      patterns(row, col) = timeS[ind - nt + 1 - lags[col]];
      sum += patterns(row, col);
    }
    sum /= NCOL;
    for (int col = 0; col < NCOL; ++col)
      patterns(row, col) /= sum;
    targets(row, _) = timeS[Range(ind - nt + 1, ind + 1)] / sum;
    row++;
  }
  List ret;
  ret["patterns"] = patterns;
  ret["targets"]  = targets;
  return ret;
}

// [[Rcpp::export]]
List build_examples_a(NumericVector timeS, NumericVector lags, int nt) {
  const int MAXLAG = lags[0];
  const int NCOL   = lags.size();
  const int NROW   = timeS.size() - MAXLAG - nt + 1;
  NumericMatrix patterns(NROW, NCOL);
  NumericMatrix targets(NROW, nt);
  IntegerVector targetsI(NROW);
  int row = 0;
  for (int ind = MAXLAG + nt -1; ind < timeS.size(); ++ind) {
    double sum = 0;
    for (int col = 0; col < NCOL; ++col) {
      patterns(row, col) = timeS[ind - nt + 1 - lags[col]];
      sum += patterns(row, col);
    }
    sum /= NCOL;
    for (int col = 0; col < NCOL; ++col)
      patterns(row, col) -= sum;
    targets(row, _) = timeS[Range(ind - nt + 1, ind + 1)] - sum;
    row++;
  }
  List ret;
  ret["patterns"] = patterns;
  ret["targets"]  = targets;
  return ret;
}
