# End-to-end exercise of the qualitative output. We do not assert exact
# wording (that would make the package brittle to copy-edits); we only
# assert that every section is reached without error.
test_that("a realistic mixed data frame produces all output sections", {
  set.seed(42)
  n <- 200L
  df <- data.frame(
    PlotObservationID = seq_len(n),
    PlotID            = sample(1:30, n, replace = TRUE),
    row_number        = seq_len(n),
    year              = sample(2000:2020, n, replace = TRUE),
    longitude         = stats::runif(n, 5, 15),
    latitude          = stats::runif(n, 40, 50),
    elevation         = stats::runif(n, 0, 2500),
    plot_area         = stats::runif(n, 1, 100),
    observer_id       = sample(letters[1:6], n, replace = TRUE),
    country           = sample(c("AT", "DE", "CH"), n, replace = TRUE),
    native_cover      = stats::runif(n, 0, 1),
    near_constant_flag = c(rep(0L, n - 2L), 1L, 2L),
    species_name      = sample(c("Quercus", "quercus", "Fagus"), n, replace = TRUE),
    stringsAsFactors  = FALSE
  )
  df$temperature <- 20 - df$elevation / 200 + stats::rnorm(n)
  df$richness    <- 5 + 0.4 * sqrt(df$plot_area) +
                    2 * (df$observer_id == "a") + stats::rnorm(n)
  df$alien_cover <- 1 - df$native_cover

  fd <- frame(df)
  expect_s3_class(fd, "frame_df")

  expect_output(print(fd), "framedf")
  expect_output(print(fd), "Structure")
  expect_output(print(fd), "Relationships")
  expect_output(print(fd), "Anomalies")
  expect_output(print(fd), "Ignored")

  expect_output(relationships(fd), "Relationships")
  expect_output(anomalies(fd),     "Anomalies")
  expect_output(details(fd),       "Details")
})
