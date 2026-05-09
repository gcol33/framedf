test_that("print.frame_df runs without error and includes the headline shape", {
  set.seed(1)
  df <- data.frame(x = 1:50, y = stats::rnorm(50))
  fd <- frame(df)
  out <- capture.output(print(fd))
  expect_true(any(grepl("^framedf", out)))
  expect_true(any(grepl("rows", out)))
  expect_true(any(grepl("^Structure",     out)))
  expect_true(any(grepl("^Relationships", out)))
  expect_true(any(grepl("^Anomalies",     out)))
})

test_that("relationships() prints meaningful section when a strong pair exists", {
  set.seed(1)
  x <- stats::rnorm(200)
  fd <- frame(data.frame(x = x, y = 0.9 * x + stats::rnorm(200, sd = 0.3)))
  out <- capture.output(relationships(fd))
  expect_true(any(grepl("meaningful", out)))
  expect_true(any(grepl("strong", out)))
})

test_that("details() prints the four sections", {
  fd <- frame(data.frame(x = 1:30, y = stats::rnorm(30)))
  out <- capture.output(details(fd))
  expect_true(any(grepl("Analysis mode", out)))
  expect_true(any(grepl("Column roles",  out)))
  expect_true(any(grepl("Skipped",       out)))
  expect_true(any(grepl("Backend",       out)))
})

test_that("anomalies() prints something when nothing is wrong", {
  fd <- frame(data.frame(x = stats::rnorm(50), y = stats::rnorm(50)))
  expect_silent({ out <- capture.output(anomalies(fd)) })
  expect_true(any(grepl("Anomalies", out)))
})

test_that("R primitive lm_simple_r recovers a known slope", {
  x <- 1:50
  y <- 2 + 3 * x + stats::rnorm(50, sd = 0.5)
  fit <- framedf:::.lm_simple_r(as.numeric(y), as.numeric(x))
  expect_equal(fit$beta1, 3, tolerance = 0.1)
  expect_gt(fit$r2, 0.99)
})
