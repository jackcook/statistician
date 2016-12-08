analyze_bivariate_data <- function(matrix, explanatory_variable, response_variable) {
    relationship <- "linear"
    r <- cor(matrix[,1], matrix[,2])

    exponential_r <- cor(matrix[,1], log(matrix[,2]))
    if (exponential_r > r) {
        relationship <- "exponential"
        r <- exponential_r
    }

    logarithmic_r <- cor(log(matrix[,1]), matrix[,2])
    if (logarithmic_r > r) {
        relationship <- "logarithmic"
        r <- logarithmic_r
    }

    power_r <- cor(log(matrix[,1]), log(matrix[,2]))
    if (power_r > r) {
        relationship <- "power"
        r <- power_r
    }

    r <- round(r, 4)

    inc_or_dec <- if (r > 0) "increase" else "decrease"

    linear_description <- ""
    if (abs(r) < 0.5) {
        linear_description <- "weakly"
    } else if (abs(r) < 0.8) {
        linear_description <- "moderately"
    } else if (abs(r) <= 1) {
        linear_description <- "strongly"
    }

    title <- paste(c(explanatory_variable, "vs.", response_variable), collapse = " ")

    plot(matrix[,1], matrix[,2], main = title, xlab = explanatory_variable, ylab = response_variable)

    if (relationship == "linear") {
        abline(lm(matrix[,2] ~ matrix[,1]))
    }

    return(paste(c("As ", explanatory_variable, " increases, ", response_variable, " tends to ", inc_or_dec, ". The association is ", linear_description, " ", relationship, ", as evidenced by r = ", r, "."), collapse = ""))
}
