shapeMultivariate <- function(values) {
    ## Test the hypothesis that two samples were drawn from populations with the same distribution

    ## Load tests
    source("./shapeMultivariate/kendall1939.R")

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    kendall1939(values))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    ## Return output
    return(output)
}
