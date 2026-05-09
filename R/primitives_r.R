# R implementations of primitives that will be replaced by C++.
# All functions here expect NA-free input; the calling layer filters first.
# Boundary: when src/primitives.cpp is compiled, these are superseded by
# lm_simple_cpp, residualize_cpp, and group_summary_cpp.

.lm_simple_r <- function(y, x) {
  n <- length(y)
  if (n < 3L) return(NULL)

  xm <- mean(x); ym <- mean(y)
  sxx <- sum((x - xm)^2)
  sxy <- sum((x - xm) * (y - ym))

  if (sxx == 0) return(NULL)
  beta1 <- sxy / sxx
  beta0 <- ym - beta1 * xm

  yhat <- beta0 + beta1 * x
  sse  <- sum((y - yhat)^2)
  sst  <- sum((y - ym)^2)
  r2   <- if (sst > 0) 1 - sse / sst else 0

  se     <- if (n > 2) sqrt((sse / (n - 2)) / sxx) else NA_real_
  t_stat <- if (!is.na(se) && se > 0) beta1 / se else NA_real_
  p_val  <- if (!is.na(t_stat)) 2 * pt(abs(t_stat), df = n - 2L, lower.tail = FALSE) else NA_real_

  list(
    beta0 = beta0, beta1 = beta1,
    se = se, t = t_stat, p = p_val,
    r2 = r2, r = sign(beta1) * sqrt(r2), n = n
  )
}

# X is a matrix (no intercept); NA-free input assumed.
.lm_multiple_r <- function(y, X) {
  n <- nrow(X); p <- ncol(X)
  if (n < p + 2L) return(NULL)

  Xd     <- cbind(1, X)
  qr_fit <- qr(Xd)
  coef   <- qr.coef(qr_fit, y)
  yhat   <- qr.fitted(qr_fit, y)
  resid  <- y - yhat

  sse <- sum(resid^2)
  sst <- sum((y - mean(y))^2)
  r2  <- if (sst > 0) 1 - sse / sst else 0

  list(coef = coef, r2 = r2, residuals = resid, n = n)
}

# Residualize y against columns of X. NA-free input assumed.
.residualize_r <- function(y, X) {
  n <- length(y); p <- ncol(X)
  if (n < p + 2L) return(rep(NA_real_, n))
  Xd <- cbind(1, X)
  y - Xd %*% qr.coef(qr(Xd), y)
}

# One-way ANOVA quantities. NA-free input assumed.
.group_summary_r <- function(y, g) {
  g <- as.character(g)
  n <- length(y)
  if (n < 2L) return(NULL)

  groups      <- unique(g)
  k           <- length(groups)
  if (k < 2L) return(NULL)
  grand_mean  <- mean(y)

  means <- tapply(y, g, mean)
  ns    <- tabulate(factor(g, levels = names(means)))
  names(ns) <- names(means)

  ss_between <- sum(ns * (means - grand_mean)^2)
  ss_total   <- sum((y - grand_mean)^2)
  eta2       <- if (ss_total > 0) ss_between / ss_total else 0

  df1 <- k - 1L; df2 <- n - k
  f_stat <- p_val <- NA_real_
  if (df1 >= 1L && df2 >= 1L) {
    ms_within <- (ss_total - ss_between) / df2
    if (ms_within > 0) {
      f_stat <- (ss_between / df1) / ms_within
      p_val  <- pf(f_stat, df1, df2, lower.tail = FALSE)
    }
  }

  list(
    means = means, ns = ns, n = n, k = k,
    eta2 = eta2, f = f_stat, p = p_val,
    grand_mean = grand_mean
  )
}
