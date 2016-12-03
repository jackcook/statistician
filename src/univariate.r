library(e1071)

source("number.r")

analyze_univariate_data <- function(data) {
  summary <- quantile(data)
  iqr <- round(summary[[4]] - summary[[2]], 4)
  lower_fence <- summary[[2]] - 1.5 * iqr
  upper_fence <- summary[[4]] + 1.5 * iqr
  
  skew <- skewness(data)
  skewed <- skew > 0.5 | skew < -0.5
  
  context <- c("stock price change", "stock price changes")
  analysis <- c()
  
  # center
  if (skewed) {
    med <- round(median(data), 4)
    analysis[length(analysis) + 1] <- paste(c("The", context[2], "have a median of", med), collapse = " ")
  } else {
    mean <- round(mean(data), 4)
    analysis[length(analysis) + 1] <- paste(c("The", context[2], "have a mean of", mean), collapse = " ")
  }
  
  # outliers
  outliers <- c()
  for (value in data) {
    if (value < lower_fence) {
      outliers[length(outliers) + 1] <- value
    } else if (value > upper_fence) {
      outliers[length(outliers) + 1] <- value
    }
  }
  
  if (length(outliers) == 1) {
    analysis[length(analysis) + 1] <- paste(c("There is one outlier of", outliers[1]), collapse = " ")
  } else {
    word <- numbers2words(length(outliers))
    outliers_list <- c(paste(outliers[1:length(outliers) - 1], collapse = ", "), "and", outliers[length(outliers)])
    analysis[length(analysis) + 1] <- paste(c("There are", word, "outliers:", outliers_list), collapse = " ")
  }
  
  # normality
  #data <- sort(rnorm(100))
  positions <- 1:length(data)
  percentiles <- ((positions - 0.5) / length(positions)) * 100
  percentile_zscores <- (percentiles - mean(percentiles)) / sd(percentiles)
  normal_probability_correlation <- cor(data, percentile_zscores)
  
  if (normal_probability_correlation > 0.95) {
    analysis[length(analysis) + 1] <- paste("The distribution of the", context[2], "appears to be normal", collapse = " ")
  }
  
  # shape
  if (skew > 1.5) {
    analysis[length(analysis) + 1] <- paste("The", context[2], "are heavily skewed to the left", collapse = " ")
  } else if (skew > 0.5) {
    analysis[length(analysis) + 1] <- paste("The", context[2], "are skewed left", collapse = " ")
  } else if (skew < -0.5) {
    analysis[length(analysis) + 1] <- paste("The", context[2], "are skewed right", collapse = " ")
  } else if (skew < -1.5) {
    analysis[length(analysis) + 1] <- paste("The", context[2], "are heavily skewed to the right", collapse = " ")
  }
  
  # spread
  if (length(outliers) == 0) {
    data_range <- range(data)[2] - range(data)[1]
    analysis[length(analysis) + 1] <- paste(c("The", context[2], "have a range of", data_range), collapse = " ")
  } else {
    analysis[length(analysis) + 1] <- paste(c("The", context[2], "have a standard deviation of", round(sd(data), 4), "and an IQR of", iqr), collapse = " ")
  }
  
  return(paste(analysis, collapse = ". "))
}