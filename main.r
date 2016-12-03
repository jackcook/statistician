setwd("/Users/jackcook/Desktop/statistician")

source("univariate.r")

data <- c(-1.44, -0.75, -0.69, -0.88, 0.12, 0.75, 0.81, -1.75, 0.69, -0.22,
          -0.16, 0.34, 0.78, 0.62, 2.44, -0.28, 2.22, -0.50, 2.06, -0.88,
          -4.50, 4.12, 1.16, -0.50)
data <- sort(data)

analysis <- analyze_univariate_data(data)
print(analysis)