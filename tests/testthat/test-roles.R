test_that("integer 1:n with id-like name is tagged as identifier", {
  df <- data.frame(plot_id = 1:30, x = stats::rnorm(30))
  fd <- frame(df)
  expect_equal(fd$roles[["plot_id"]], "id")
})

test_that("integer 1:n with row_number-like name is admin_index", {
  df <- data.frame(row_number = 1:30, x = stats::rnorm(30))
  fd <- frame(df)
  expect_equal(fd$roles[["row_number"]], "admin_index")
})

test_that("repeated id-like integer becomes a group_id", {
  df <- data.frame(PlotID = sample(1:5, 50, replace = TRUE),
                   y      = stats::rnorm(50))
  fd <- frame(df)
  expect_equal(fd$roles[["PlotID"]], "group_id")
})

test_that("year-like integer is detected as temporal", {
  df <- data.frame(year = sample(1990:2020, 100, replace = TRUE),
                   y    = stats::rnorm(100))
  fd <- frame(df)
  expect_equal(fd$roles[["year"]], "temporal")
})

test_that("latitude / longitude detection is name-and-range based", {
  df <- data.frame(
    latitude  = stats::runif(50, 40, 50),
    longitude = stats::runif(50, 5, 15),
    y         = stats::rnorm(50)
  )
  fd <- frame(df)
  expect_equal(fd$roles[["latitude"]],  "coord_lat")
  expect_equal(fd$roles[["longitude"]], "coord_lon")
})

test_that("a few impossible coord values do not disqualify the role", {
  lat <- stats::runif(100, 40, 50)
  lat[c(1, 2)] <- c(120, -100)
  fd <- frame(data.frame(latitude = lat, y = stats::rnorm(100)))
  expect_equal(fd$roles[["latitude"]], "coord_lat")
})

test_that("compositional cover columns get the compositional role", {
  df <- data.frame(
    native_cover = stats::runif(50, 0, 1),
    alien_cover  = stats::runif(50, 0, 1),
    other        = stats::rnorm(50)
  )
  fd <- frame(df)
  expect_equal(fd$roles[["native_cover"]], "compositional")
  expect_equal(fd$roles[["alien_cover"]],  "compositional")
  expect_equal(fd$roles[["other"]],        "measurement")
})

test_that("constant and near-constant columns are flagged", {
  df <- data.frame(
    k = rep(1, 50),
    nc = c(rep(0, 49), 1),
    y  = stats::rnorm(50)
  )
  fd <- frame(df)
  expect_equal(fd$roles[["k"]],  "constant")
  expect_equal(fd$roles[["nc"]], "near_constant")
})
