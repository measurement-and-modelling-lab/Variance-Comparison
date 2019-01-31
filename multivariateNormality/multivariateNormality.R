multivariateNormality <- function(data)
{
    ## Load tests
    source("./multivariateNormality/mardia1970.R")

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    mardia1970(data))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    ## Return output
    return(output)

}
