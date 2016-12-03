analyze_bivariate_data <- function(matrix, explanatory_variable, response_variable) {
  r <- round(cor(matrix[,1], matrix[,2]), 4)
  inc_or_dec <- if (r > 0) "increase" else "decrease"
  
  linear_description <- ""
  if (abs(r) < 0.5) {
    linear_description <- "weakly"
  } else if (abs(r) < 0.8) {
    linear_description <- "moderately"
  } else if (abs(r) <= 1) {
    linear_description <- "strongly"
  }
  
  paste(c("As ", explanatory_variable, " increases, ", response_variable, " tends to ", inc_or_dec, ". The association is ", linear_description, " linear, as evidenced by r = ", r, "."), collapse = "")
}