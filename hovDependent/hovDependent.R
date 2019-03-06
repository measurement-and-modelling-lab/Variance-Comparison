hovDependent <- function(data)
{
    ## Test homogeneity of variances on several variables within a single group

    ## Load tests
    source("./hovDependent/homogeneityTest.R")
    dyn.load("./hovDependent/studentFit", local=FALSE)

    ## Prepare data
    sf <- studentFit(data)

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    homogeneityTest(sf, test="LRT"),
                    homogeneityTest(sf, test="Wald"),
                    homogeneityTest(sf, test="score"),
                    homogeneityTest(sf, test="gradient"))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    return(output)
}
