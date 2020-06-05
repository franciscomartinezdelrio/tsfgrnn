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
      w[i] += 1 / w.size();
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
