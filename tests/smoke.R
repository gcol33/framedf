library(framedf)
set.seed(42)
n <- 80L
df <- data.frame(
  row_id            = 1:n,
  PlotObservationID = as.integer(sample(1000:9999, n)),
  subject_id        = paste0("S", seq_len(n)),
  height            = rnorm(n, 170, 10),
  weight            = rnorm(n, 70,  12),
  bmi               = rnorm(n, 24,  3),
  age               = rnorm(n, 45,  12),
  sex               = sample(c("M","F"), n, replace = TRUE),
  group             = sample(c("A","B","C"), n, replace = TRUE),
  constant_col      = rep(1.0, n),
  near_constant_flag = c(rep(0, n - 2L), 1, 2),
  stringsAsFactors  = FALSE
)

fd <- frame(df)
print(fd)

cat("\n--- relationships() ---\n")
relationships(fd, min_strength = "moderate")

cat("\n--- details() ---\n")
details(fd)
