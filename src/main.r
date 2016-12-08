setwd("/Users/jackcook/Desktop/statistician/src")

source("bivariate.r")
source("univariate.r")

cat("What type of data would you like to analyze? (1 = univariate, 2 = bivariate, 3 = categorical): ")
data_type <- readLines(con = "stdin", 1)

if (data_type == 1) {
  cat("What is being measured? ")
  name <- readLines(con = "stdin", 1)

  cat("Enter in your data values, separated by spaces: ")
  data_points <- readLines(con = "stdin", 1)

  values <- as.numeric(strsplit(data_points, " ")[[1]])
  analysis <- analyze_univariate_data(values, name)
  print(analysis)
} else if (data_type == 2) {
  cat("What is your explanatory variable measuring? ")
  explanatory_name <- readLines(con = "stdin", 1)

  cat("What is your response variable measuring? ")
  response_name <- readLines(con = "stdin", 1)

  cat("Enter in your explanatory variable values, separated by spaces: ")
  explanatory_variable <- readLines(con = "stdin", 1)

  cat("Enter in your response variable values, separated by spaces: ")
  response_variable <- readLines(con = "stdin", 1)

  explanatory_values <- as.numeric(strsplit(explanatory_variable, " ")[[1]])
  response_values <- as.numeric(strsplit(response_variable, " ")[[1]])

  data <- matrix(cbind(explanatory_values, response_values), ncol = 2)
  analysis <- analyze_bivariate_data(data, explanatory_name, response_name)
  print(analysis)
} else if (data_type == 3) {
  cat("What are your categories? ")
  categories_name <- readLines(con = "stdin", 1)

  cat("Enter in your categories' names, separated by spaces: ")
  categories <- readLines(con = "stdin", 1)

  cat("Enter in your categories' values, separated by spaces: ")
  values <- readLines(con = "stdin", 1)

  category_names <- strsplit(categories, " ")[[1]]
  category_values <- as.numeric(strsplit(values, " ")[[1]])

  percentages <- round(category_values / sum(category_values) * 100)

  lbls <- category_names
  lbls <- paste(lbls, percentages)
  lbls <- paste(lbls, "%", sep = "")

  pie(category_values, labels = lbls, main = categories_name)
}
