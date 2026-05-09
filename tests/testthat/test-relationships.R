test_that("a strong linear pair is flagged as meaningful and strong", {
  set.seed(1)
  x <- stats::rnorm(200)
  df <- data.frame(x = x, y = 0.9 * x + stats::rnorm(200, sd = 0.3))
  fd <- frame(df)
  meas <- Filter(function(f) f$type == "numeric_numeric",
                 fd$relationship_findings)
  expect_length(meas, 1L)
  expect_equal(meas[[1]]$strength, "strong")
  expect_equal(meas[[1]]$kind, "meaningful")
})

test_that("a categorical with id-like name and strong group effect is suspicious", {
  set.seed(1)
  obs <- sample(letters[1:8], 400, replace = TRUE)
  effect <- stats::setNames(stats::rnorm(8, 0, 5), letters[1:8])
  y <- effect[obs] + stats::rnorm(400)
  df <- data.frame(observer_id = obs, y = as.numeric(y))
  fd <- frame(df)
  cat_findings <- Filter(function(f) f$type == "categorical_numeric",
                         fd$relationship_findings)
  expect_true(length(cat_findings) >= 1L)
  expect_equal(cat_findings[[1]]$kind, "suspicious")
  expect_equal(cat_findings[[1]]$concern, "possible observer effect")
})

test_that("temporal × spatial drift fires when present", {
  set.seed(1)
  yr <- sample(2000:2020, 500, replace = TRUE)
  df <- data.frame(year = yr,
                   longitude = 10 + (yr - 2010) * 0.3 + stats::rnorm(500, sd = 0.1),
                   y = stats::rnorm(500))
  fd <- frame(df)
  drift <- Filter(function(f) f$type == "drift", fd$relationship_findings)
  expect_true(length(drift) >= 1L)
  expect_true(drift[[1]]$kind %in% c("suspicious", "negligible"))
})

test_that("constrained complement pairs are flagged as structural", {
  set.seed(1)
  a <- stats::runif(200, 0, 1)
  df <- data.frame(native_cover = a, alien_cover = 1 - a)
  fd <- frame(df)
  comp <- Filter(function(f) f$type == "compositional",
                 fd$relationship_findings)
  expect_length(comp, 1L)
  expect_equal(comp[[1]]$kind, "structural")
})

test_that("ignored columns are excluded from the numeric pair sweep", {
  df <- data.frame(plot_id = 1:50, x = stats::rnorm(50), y = stats::rnorm(50))
  fd <- frame(df)
  expect_true("plot_id" %in% fd$ignored_cols)
  pair_x <- vapply(fd$relationship_findings, `[[`, character(1L), "x")
  pair_y <- vapply(fd$relationship_findings, `[[`, character(1L), "y")
  expect_false("plot_id" %in% c(pair_x, pair_y))
})

test_that("adjustment shrinks an apparent correlation through a confounder", {
  set.seed(1)
  z <- stats::rnorm(200)
  df <- data.frame(
    x = z + stats::rnorm(200, sd = 1.5),
    y = z + stats::rnorm(200, sd = 1.5),
    z = z
  )
  raw <- frame(df)
  adj <- frame(df, adjustment = "z")

  raw_finding <- raw$relationship_findings[[
    which(vapply(raw$relationship_findings,
                 function(f) all(c("x", "y") %in% c(f$x, f$y)),
                 logical(1L)))[1L]
  ]]
  adj_finding <- adj$relationship_findings[[
    which(vapply(adj$relationship_findings,
                 function(f) all(c("x", "y") %in% c(f$x, f$y)),
                 logical(1L)))[1L]
  ]]
  expect_gt(abs(raw_finding$evidence$r), abs(adj_finding$evidence$r))
})
