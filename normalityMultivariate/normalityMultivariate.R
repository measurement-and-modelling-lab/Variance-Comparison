normalityMultivariate <- function(data) {
    ## Load tests
    source("./normalityMultivariate/doornik2008.R")
    source("./normalityMultivariate/henze1990.R")
    source("./normalityMultivariate/mardia1970.R")
    source("./normalityMultivariate/royston1982.R")
    source("./normalityMultivariate/shapiro1965.R")

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    doornik2008(data),
                    henze1990(data),
                    mardia1970_kurtosis(data),
                    mardia1970_skewness(data),
                    royston1982(data),
                    shapiro1965(data))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    ## Return output
    return(output)

}
