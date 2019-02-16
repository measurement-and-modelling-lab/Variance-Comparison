shapeUnivariate <- function(values, groups)
{
    ## Test the hypothesis that two samples were drawn from populations with the same distribution

    ## Load tests
    source("./shapeUnivariate/jonckheere1954.R")
    source("./shapeUnivariate/kruskal1952.R")
    source("./shapeUnivariate/wilcoxon1945.R")

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    jonckheere1954(values, groups),
                    kruskal1952(values, groups),
                    wilcoxon1945(values, groups))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    ## Return output
    return(output)
}
