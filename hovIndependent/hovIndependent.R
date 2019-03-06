hovIndependent <- function(values, groups)
{
    ## Test homogeneity of variances on a single variable across two or more groups

    ## Load tests
    source("./hovIndependent/bartlett1937.R")
    source("./hovIndependent/fligner1976.R")
    source("./hovIndependent/levene1960.R")

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    bartlett1937(values, groups),
                    fligner1976(values, groups),
                    levene1960(values, groups))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    ## Return output
    return(output)
}
