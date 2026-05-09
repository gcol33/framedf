test_that("Tukey-fence outliers are detected on a long-tailed measurement", {
  set.seed(1)
  y <- c(stats::rnorm(98), 100, -100)
  fd <- frame(data.frame(y = y))
  out <- Filter(function(f) f$type == "outlier", fd$anomaly_findings)
  expect_length(out, 1L)
  expect_equal(out[[1]]$column, "y")
  expect_gte(out[[1]]$n, 2L)
})

test_that("strongly skewed measurement is flagged", {
  set.seed(1)
  y <- stats::rexp(200, rate = 1)  # right skewed
  fd <- frame(data.frame(y = y))
  skew <- Filter(function(f) f$type == "skewness", fd$anomaly_findings)
  expect_true(length(skew) >= 1L)
  expect_equal(skew[[1]]$direction, "right")
})

test_that("implausible coordinate values are reported", {
  lat <- stats::runif(100, 40, 50)
  lat[c(1, 2)] <- c(120, -200)
  fd <- frame(data.frame(latitude = lat))
  imp <- Filter(function(f) f$type == "implausible_range", fd$anomaly_findings)
  expect_length(imp, 1L)
  expect_equal(imp[[1]]$column, "latitude")
})

test_that("inconsistent capitalization is detected", {
  fd <- frame(data.frame(
    species = c(rep("Quercus robur", 50), rep("quercus robur", 50))
  ))
  ic <- Filter(function(f) f$type == "case_inconsistency", fd$anomaly_findings)
  expect_length(ic, 1L)
})

test_that("rare categorical levels are detected", {
  fd <- frame(data.frame(
    treatment = c(rep("X", 100), rep("Y", 100), "Z")
  ))
  rare <- Filter(function(f) f$type == "rare_levels", fd$anomaly_findings)
  expect_length(rare, 1L)
  expect_equal(rare[[1]]$n_rare, 1L)
})

test_that("compositional pair exceeding 1.0 is flagged", {
  set.seed(1)
  a <- stats::runif(50, 0.7, 1.1)
  b <- stats::runif(50, 0.5, 0.9)
  fd <- frame(data.frame(native_cover = a, alien_cover = b))
  ex <- Filter(function(f) f$type == "pair_exceeds_total", fd$anomaly_findings)
  expect_true(length(ex) >= 1L)
})
