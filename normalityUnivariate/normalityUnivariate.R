normalityUnivariate <- function(data) {
    ## Test the normality of a vector of scores

    ## Load tests
    source("./normalityUnivariate/agostino1971.R")
    source("./normalityUnivariate/agostino1973.R")
    source("./normalityUnivariate/anderson1954.R")
    source("./normalityUnivariate/barrio1999.R")
    source("./normalityUnivariate/bonett2002.R")
    source("./normalityUnivariate/bonettseier2002.R")
    source("./normalityUnivariate/bontemps2005a.R")
    source("./normalityUnivariate/bontemps2005b.R")
    source("./normalityUnivariate/cabana1994a.R")
    source("./normalityUnivariate/cabana1994b.R")
    source("./normalityUnivariate/chen1995.R")
    source("./normalityUnivariate/coin2008.R")
    source("./normalityUnivariate/cramer1928.R")
    source("./normalityUnivariate/desgagne2013.R")
    source("./normalityUnivariate/desgagne2018a.R")
    source("./normalityUnivariate/desgagne2018b.R")
    source("./normalityUnivariate/doornik1994.R")
    source("./normalityUnivariate/epps1983.R")
    source("./normalityUnivariate/filliben1975.R")
    source("./normalityUnivariate/gel2007.R")
    source("./normalityUnivariate/gel2008.R")
    source("./normalityUnivariate/glen2001.R")
    source("./normalityUnivariate/hosking1990a.R")
    source("./normalityUnivariate/hosking1990b.R")
    source("./normalityUnivariate/hosking1990c.R")
    source("./normalityUnivariate/hosking1990d.R")
    source("./normalityUnivariate/jarque1980.R")
    source("./normalityUnivariate/lilliefors1967.R")
    source("./normalityUnivariate/martinez1981.R")
    source("./normalityUnivariate/pearson1900.R")
    source("./normalityUnivariate/rahman1997.R")
    source("./normalityUnivariate/shapiro1965.R")
    source("./normalityUnivariate/shapiro1972.R")
    source("./normalityUnivariate/spiegelhalter1977.R")
    source("./normalityUnivariate/zhang1999a.R")
    source("./normalityUnivariate/zhang1999b.R")
    source("./normalityUnivariate/zhang2005a.R")
    source("./normalityUnivariate/zhang2005b.R")

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    agostino1971(data),
                    agostino1973(data),
                    anderson1954(data),
                    barrio1999(data),
                    bonett2002(data),
                    bonettseier2002(data),
                    bontemps2005a(data),
                    bontemps2005b(data),
                    cabana1994a(data),
                    cabana1994b(data),
                    chen1995(data),
                    coin2008(data),
                    cramer1928(data),
                    desgagne2013(data), 
                    desgagne2018a(data), 
                    desgagne2018b(data), 
                    doornik1994(data),
                    epps1983(data),
                    filliben1975(data),
                    gel2007(data), 
                    gel2008(data),
                    glen2001(data), 
                    hosking1990a(data),
                    hosking1990b(data),
                    hosking1990c(data),
                    hosking1990d(data),
                    jarque1980(data),
                    lilliefors1967(data),
                    martinez1981(data),
                    pearson1900(data),
                    rahman1997(data),
                    shapiro1965(data),
                    shapiro1972(data),
                    spiegelhalter1977(data)
                    zhang1999a(data),
                    zhang1999b(data),
                    zhang2005a(data),
                    zhang2005b(data))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    ## Return output
    return(output)
    
}
