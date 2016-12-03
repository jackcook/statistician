setwd("/Users/jackcook/Desktop/statistician/src")

source("bivariate.r")
source("univariate.r")

data_type <- readline(prompt = "What type of data would you like to analyze? (1 = univariate, 2 = bivariate, 3 = categorical): ")

if (data_type == 1) {
  name <- readline(prompt = "What is being measured? ")
  data_points <- readline(prompt = "Enter in your data values, separated by spaces: ")
  values <- as.numeric(strsplit(data_points, " ")[[1]])
  analysis <- analyze_univariate_data(values, name)
  print(analysis)
} else if (data_type == 2) {
  explanatory_name <- readline(prompt = "What is your explanatory variable measuring? ")
  response_name <- readline(prompt = "What is your response variable measuring? ")
  
  explanatory_variable <- readline(prompt = "Enter in your explanatory variable values, separated by spaces: ")
  response_variable <- readline(prompt = "Enter in your response variable values, separated by spaces: ")
  
  explanatory_values <- as.numeric(strsplit(explanatory_variable, " ")[[1]])
  response_values <- as.numeric(strsplit(response_variable, " ")[[1]])
  
  data <- matrix(cbind(explanatory_values, response_values), ncol = 2)
  analysis <- analyze_bivariate_data(data, explanatory_name, response_name)
  print(analysis)
} else if (data_type == 3) {
  categories_name <- readline(prompt = "What are your categories? ")
  categories <- readline(prompt = "Enter in your category names, separated by spaces: ")
  values <- readline(prompt = "Enter in your categories' values, separated by spaces: ")
  
  category_names <- strsplit(categories, " ")[[1]]
  category_values <- as.numeric(strsplit(values, " ")[[1]])
  
  percentages <- round(category_values / sum(category_values) * 100)
  
  lbls <- category_names
  lbls <- paste(lbls, percentages)
  lbls <- paste(lbls, "%", sep = "")
  
  pie(category_values, labels = lbls, main = categories_name)
}