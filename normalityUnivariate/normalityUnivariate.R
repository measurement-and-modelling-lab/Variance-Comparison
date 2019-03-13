normalityUnivariate <- function(data) {
    ## Test the normality of a vector of scores

    ## Load tests
    source("./normalityUnivariate/agostino1973.R")
    source("./normalityUnivariate/anderson1954.R")
    source("./normalityUnivariate/bonett2002.R")
    source("./normalityUnivariate/cramer1928.R")
    source("./normalityUnivariate/doornik1994.R")
    source("./normalityUnivariate/gel2008.R")
    source("./normalityUnivariate/jarque1980.R")
    source("./normalityUnivariate/lilliefors1967.R")
    source("./normalityUnivariate/pearson1900.R")
    source("./normalityUnivariate/shapiro1965.R")
    source("./normalityUnivariate/shapiro1972.R")
    source("./normalityUnivariate/gelgastwirth2008.R")
    source("./normalityUnivariate/hosking1990a.R")
    source("./normalityUnivariate/hosking1990b.R")
    source("./normalityUnivariate/hosking1990c.R")
    source("./normalityUnivariate/hosking1990d.R")
    source("./normalityUnivariate/bontempsmeddahi2005a.R")
    source("./normalityUnivariate/bontempsmeddahi2005b.R")
    source("./normalityUnivariate/bonettseier2002.R")

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
                    shapiro1972(data),
                    gelgastwirth2008(data),
                    hosking1990a(data),
                    hosking1990b(data),
                    hosking1990c(data),
                    hosking1990d(data),
                    bontempsmeddahi2005a(data),
                    bontempsmeddahi2005b(data),
                    bonettseier2002(data))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    ## Return output
    return(output)
    
}