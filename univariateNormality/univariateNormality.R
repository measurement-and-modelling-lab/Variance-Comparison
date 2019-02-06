univariateNormality <- function(data) {
    ## Test the normality of a vector of scores

    ## Load tests
    source("./univariateNormality/agostino1973.R")
    source("./univariateNormality/anderson1954.R")
    source("./univariateNormality/bonett2002.R")
    source("./univariateNormality/cramer1928.R")
    source("./univariateNormality/doornik1994.R")
    source("./univariateNormality/gel2008.R")
    source("./univariateNormality/jarque1980.R")
    source("./univariateNormality/lilliefors1967.R")
    source("./univariateNormality/pearson1900.R")
    source("./univariateNormality/shapiro1965.R")
    source("./univariateNormality/shapiro1972.R")

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    agostino1973(data),
                    anderson1954(data),
                    bonett2002(data),
                    cramer1928(data),
                    doornik1994(data),
                    gel2008(data),
                    jarque1980(data),
                    lilliefors1967(data),
                    pearson1900(data),
                    shapiro1965(data),
                    shapiro1972(data))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    ## Return output
    return(output)

}
