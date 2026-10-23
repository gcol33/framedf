test_that("triage() returns an object of class frame_df", {
  fd <- triage(data.frame(x = 1:30, y = stats::rnorm(30)))
  expect_s3_class(fd, "frame_df")
})

test_that("triage() rejects non data frames", {
  expect_error(triage(1:10), "data frame")
})

test_that("framedf_settings() returns a named list with sane defaults", {
  s <- framedf_settings()
  expect_type(s, "list")
  expect_true(all(c("min_obs", "strong_threshold", "moderate_threshold",
                    "weak_threshold", "subsample_threshold") %in% names(s)))
  expect_true(s$strong_threshold > s$moderate_threshold)
  expect_true(s$moderate_threshold > s$weak_threshold)
})

test_that("framedf_settings() accepts overrides through ...", {
  s <- framedf_settings(strong_threshold = 0.7)
  expect_equal(s$strong_threshold, 0.7)
})

test_that("framedf exports no name that masks base or recommended packages", {
  ours <- getNamespaceExports("framedf")
  theirs <- unlist(lapply(
    c("base", "stats", "utils", "graphics", "grDevices", "methods"),
    getNamespaceExports
  ))
  expect_length(intersect(ours, theirs), 0L)
})
