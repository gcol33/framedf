test_that("frame() returns an object of class frame_df", {
  fd <- frame(data.frame(x = 1:30, y = stats::rnorm(30)))
  expect_s3_class(fd, "frame_df")
})

test_that("frame() rejects non data frames", {
  expect_error(frame(1:10), "data frame")
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
