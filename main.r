library(e1071)

data <- c(-1.44, -0.75, -0.69, -0.88, 0.12, 0.75, 0.81, -1.75, 0.69, -0.22, -0.16, 0.34, 0.78, 0.62, 2.44, -0.28, 2.22, -0.50, 2.06, -0.88, -4.50, 4.12, 1.16, -0.50)

summary <- quantile(data)
iqr <- summary[[4]] - summary[[2]]
lower_fence <- summary[[2]] - 1.5 * iqr
upper_fence <- summary[[4]] + 1.5 * iqr

skew <- skewness(data)
skewed <- skew > 0.5 | skew < -0.5

context <- c("email", "emails")

# center
if (skewed) {
  med <- round(median(data), 4)
  paste(c("The", context[2], "have a median of", med), collapse = " ")
} else {
  mean <- round(mean(data), 4)
  paste(c("The", context[2], "have a mean of", mean), collapse = " ")
}