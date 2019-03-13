hovDependent <- function(data)
{
    ## Test homogeneity of variances on several variables within a single group

    ## Load tests
    source("./hovDependent/harris1985a.R")
    source("./hovDependent/harris1985b.R")
    source("./hovDependent/harris1985c.R")
    source("./hovDependent/harris1985d.R")
    dyn.load("./hovDependent/studentFit", local=FALSE)

    ## Prepare data
    sf <- studentFit(data)

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    harris1985a(sf),
                    harris1985b(sf),
                    harris1985c(sf),
                    harris1985d(sf))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    return(output)
}