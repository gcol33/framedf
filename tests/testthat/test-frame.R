set.seed(42)
n <- 80L
df <- data.frame(
  row_id          = 1:n,                              # admin_index
  subject_id      = paste0("S", 1:n),                 # id
  height          = rnorm(n, 170, 10),                 # measurement
  weight          = rnorm(n, 70,  12),                 # measurement (corr with bmi)
  bmi             = rnorm(n, 24,  3),                  # measurement
  age             = rnorm(n, 45,  12),                 # measurement
  sex             = sample(c("M","F"), n, replace=TRUE), # categorical
  group           = sample(c("A","B","C"), n, replace=TRUE), # categorical
  constant_col    = rep(1.0, n),                       # constant
  near_zero       = c(rep(0, n-2L), 1, 2),             # near_constant
  stringsAsFactors = FALSE
)

test_that("frame() returns a frame_df", {
  fd <- frame(df)
  expect_s3_class(fd, "frame_df")
})

test_that("roles are inferred correctly", {
  fd <- frame(df)
  expect_equal(fd$roles[["row_id"]],       "admin_index")
  expect_equal(fd$roles[["subject_id"]],   "id")
  expect_equal(fd$roles[["constant_col"]], "constant")
  expect_equal(fd$roles[["sex"]],          "categorical")
  expect_equal(fd$roles[["height"]],       "measurement")
})

test_that("ignored_cols excludes non-screening roles", {
  fd <- frame(df)
  expect_true("row_id"       %in% fd$ignored_cols)
  expect_true("subject_id"   %in% fd$ignored_cols)
  expect_true("constant_col" %in% fd$ignored_cols)
})

test_that("relationship_findings are present", {
  fd <- frame(df)
  expect_type(fd$relationship_findings, "list")
})

test_that("print.frame_df runs without error", {
  fd <- frame(df)
  expect_output(print(fd), "frame_df")
})

test_that("relationships() returns invisible list", {
  fd <- frame(df)
  out <- capture.output(res <- relationships(fd))
  expect_type(res, "list")
})

test_that("anomalies() runs without error", {
  fd <- frame(df)
  expect_no_error(anomalies(fd))
})

test_that("details() runs without error", {
  fd <- frame(df)
  expect_output(details(fd), "frame_df details")
})

test_that("R primitives: lm_simple_r recovers known slope", {
  x <- 1:50
  y <- 2 + 3 * x + rnorm(50, sd = 0.5)
  fit <- framedf:::.lm_simple_r(as.numeric(y), as.numeric(x))
  expect_equal(fit$beta1, 3, tolerance = 0.1)
  expect_gt(fit$r2, 0.99)
})

test_that("R primitives: group_summary_r recovers eta2 > 0 for separated groups", {
  g <- rep(c("A","B"), each = 30L)
  y <- c(rnorm(30, 0, 1), rnorm(30, 5, 1))
  fit <- framedf:::.group_summary_r(y, g)
  expect_gt(fit$eta2, 0.8)
  expect_lt(fit$p, 0.001)
})

test_that("adjustment removes a known confounder", {
  z <- rnorm(100)            # confounder
  x <- z + rnorm(100, sd=2)
  y <- z + rnorm(100, sd=2)  # x and y correlated only via z
  df2 <- data.frame(x = x, y = y, z = z)
  fd_adj  <- frame(df2, adjustment = "z")
  fd_raw  <- frame(df2)
  # Unadjusted should show stronger relationship than adjusted
  r_raw <- fd_raw$relationship_findings[[1]]$evidence$r
  r_adj <- fd_adj$relationship_findings[[1]]$evidence$r
  expect_gt(abs(r_raw), abs(r_adj))
})
