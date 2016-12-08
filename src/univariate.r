library(e1071)

source("number.r")

analyze_univariate_data <- function(data, context) {
  #data <- c(-1.44, -0.75, -0.69, -0.88, 0.12, 0.75, 0.81, -1.75, 0.69, -0.22,
  #          -0.16, 0.34, 0.78, 0.62, 2.44, -0.28, 2.22, -0.50, 2.06, -0.88,
  #          -4.50, 4.12, 1.16, -0.50)
  
  analysis <<- c()
  
  add_analysis <- function(text) {
    analysis[length(analysis) + 1] <<- text
  }
  
  data <- sort(data)
  
  summary <- quantile(data)
  iqr <- round(summary[[4]] - summary[[2]], 4)
  lower_fence <- summary[[2]] - 1.5 * iqr
  upper_fence <- summary[[4]] + 1.5 * iqr
  
  skew <- skewness(data)
  skewed <- skew > 0.5 | skew < -0.5
  
  # center
  if (skewed) {
    med <- round(median(data), 4)
    add_analysis(paste(c("The", context, "have a median of", med), collapse = " "))
  } else {
    mean <- round(mean(data), 4)
    add_analysis(paste(c("The", context, "have a mean of", mean), collapse = " "))
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
    add_analysis(paste(c("There is one outlier of", outliers[1]), collapse = " "))
  } else if (length(outliers) > 1) {
    word <- numbers2words(length(outliers))
    outliers_list <- c(paste(outliers[1:length(outliers) - 1], collapse = ", "), "and", outliers[length(outliers)])
    add_analysis(paste(c("There are", word, "outliers:", outliers_list), collapse = " "))
  }
  
  # normality
  #data <- sort(rnorm(100))
  positions <- 1:length(data)
  percentiles <- ((positions - 0.5) / length(positions)) * 100
  percentile_zscores <- (percentiles - mean(percentiles)) / sd(percentiles)
  normal_probability_correlation <- cor(data, percentile_zscores)
  
  if (normal_probability_correlation > 0.975) {
    add_analysis(paste("The distribution of the", context, "appears to be normal", collapse = " "))
  }
  
  # shape
  if (skew > 1.5) {
    add_analysis(paste("The", context, "are heavily skewed to the right", collapse = " "))
  } else if (skew > 0.5) {
    add_analysis(paste("The", context, "are skewed right", collapse = " "))
  } else if (skew < -0.5) {
    add_analysis(paste("The", context, "are skewed left", collapse = " "))
  } else if (skew < -1.5) {
    add_analysis(paste("The", context, "are heavily skewed to the left", collapse = " "))
  }
  
  # spread
  if (length(outliers) == 0) {
    range <- max(data) - min(data)
    add_analysis(paste(c("The", context, "have a range of", range), collapse = " "))
  } else {
    add_analysis(paste(c("The", context, "have a standard deviation of", round(sd(data), 4), "and an IQR of", iqr), collapse = " "))
  }
  
  hist(data)
  return(paste(c(paste(analysis, collapse = ". "), "."), collapse = ""))
}