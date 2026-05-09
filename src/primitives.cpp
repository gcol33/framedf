#include <Rcpp.h>
#include <map>
#include <string>
#include <cmath>
using namespace Rcpp;

// ---- simple linear regression (y ~ x) ----------------------------------
// Input must be NA-free; the R calling layer filters first.
// [[Rcpp::export]]
List lm_simple_cpp(NumericVector y, NumericVector x) {
  int n = y.size();
  if (n < 3) return R_NilValue;

  double xm = 0.0, ym = 0.0;
  for (int i = 0; i < n; ++i) { xm += x[i]; ym += y[i]; }
  xm /= n; ym /= n;

  double sxx = 0.0, sxy = 0.0;
  for (int i = 0; i < n; ++i) {
    double dx = x[i] - xm;
    sxx += dx * dx;
    sxy += dx * (y[i] - ym);
  }
  if (sxx == 0.0) return R_NilValue;

  double beta1 = sxy / sxx;
  double beta0 = ym - beta1 * xm;

  double sse = 0.0, sst = 0.0;
  for (int i = 0; i < n; ++i) {
    double resid = y[i] - (beta0 + beta1 * x[i]);
    double dev   = y[i] - ym;
    sse += resid * resid;
    sst += dev   * dev;
  }

  double r2     = (sst > 0.0) ? 1.0 - sse / sst : 0.0;
  double se     = (n > 2 && sxx > 0.0) ? std::sqrt((sse / (n - 2)) / sxx) : NA_REAL;
  double t_stat = (!ISNAN(se) && se > 0.0) ? beta1 / se : NA_REAL;

  double p_val = NA_REAL;
  if (!ISNAN(t_stat)) {
    Function pt("pt");
    p_val = as<double>(pt(std::abs(t_stat),
                          Named("df")         = n - 2,
                          Named("lower.tail") = false)) * 2.0;
  }

  return List::create(
    Named("beta0") = beta0,
    Named("beta1") = beta1,
    Named("se")    = se,
    Named("t")     = t_stat,
    Named("p")     = p_val,
    Named("r2")    = r2,
    Named("r")     = std::copysign(std::sqrt(r2), beta1),
    Named("n")     = n
  );
}

// ---- residualize y against columns of X (intercept added internally) -----
// Uses R's QR for numerical stability. Boundary: replace with LAPACK dgelsd
// when we want to eliminate the R callback overhead.
// Input must be NA-free; n == nrow(X) == length(y).
// [[Rcpp::export]]
NumericVector residualize_cpp(NumericVector y, NumericMatrix X) {
  int n = y.size();
  int p = X.ncol();
  NumericVector out(n, NA_REAL);
  if (n < p + 2) return out;

  // Build [1 | X]
  NumericMatrix Xd(n, p + 1);
  for (int i = 0; i < n; ++i) Xd(i, 0) = 1.0;
  for (int j = 0; j < p; ++j)
    for (int i = 0; i < n; ++i)
      Xd(i, j + 1) = X(i, j);

  Function qr_fn("qr");
  Function qr_fitted("qr.fitted");
  SEXP     qr_obj = qr_fn(Xd);
  NumericVector fitted = qr_fitted(qr_obj, y);

  for (int i = 0; i < n; ++i) out[i] = y[i] - fitted[i];
  return out;
}

// ---- one-way ANOVA quantities (eta², F, group means) --------------------
// Input must be NA-free.
// [[Rcpp::export]]
List group_summary_cpp(NumericVector y, CharacterVector g) {
  int n = y.size();
  if (n < 2) return R_NilValue;

  // Assign each observed group an integer index.
  std::map<std::string, int> group_idx;
  std::vector<std::string>   group_names;
  for (int i = 0; i < n; ++i) {
    std::string gi = as<std::string>(g[i]);
    if (group_idx.find(gi) == group_idx.end()) {
      group_idx[gi] = (int)group_names.size();
      group_names.push_back(gi);
    }
  }
  int k = (int)group_names.size();
  if (k < 2) return R_NilValue;

  std::vector<double> sums(k, 0.0);
  std::vector<int>    counts(k, 0);
  for (int i = 0; i < n; ++i) {
    int gi = group_idx[as<std::string>(g[i])];
    sums[gi]   += y[i];
    counts[gi] += 1;
  }

  NumericVector means(k); IntegerVector ns(k);
  for (int j = 0; j < k; ++j) {
    means[j] = sums[j] / counts[j];
    ns[j]    = counts[j];
  }
  means.attr("names") = wrap(group_names);
  ns.attr("names")    = wrap(group_names);

  double grand_mean = 0.0;
  for (int i = 0; i < n; ++i) grand_mean += y[i];
  grand_mean /= n;

  double ss_between = 0.0, ss_total = 0.0;
  for (int j = 0; j < k; ++j)
    ss_between += counts[j] * (means[j] - grand_mean) * (means[j] - grand_mean);
  for (int i = 0; i < n; ++i)
    ss_total += (y[i] - grand_mean) * (y[i] - grand_mean);

  double eta2 = (ss_total > 0.0) ? ss_between / ss_total : 0.0;

  int    df1 = k - 1, df2 = n - k;
  double f_stat = NA_REAL, p_val = NA_REAL;
  if (df1 >= 1 && df2 >= 1) {
    double ms_within = (ss_total - ss_between) / df2;
    if (ms_within > 0.0) {
      f_stat = (ss_between / df1) / ms_within;
      Function pf_fn("pf");
      p_val = as<double>(pf_fn(f_stat, df1, df2, Named("lower.tail") = false));
    }
  }

  return List::create(
    Named("means")      = means,
    Named("ns")         = ns,
    Named("n")          = n,
    Named("k")          = k,
    Named("eta2")       = eta2,
    Named("f")          = f_stat,
    Named("p")          = p_val,
    Named("grand_mean") = grand_mean
  );
}
