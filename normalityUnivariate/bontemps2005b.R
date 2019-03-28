bontemps2005b <- function(x) {
    ## The 2nd Bontemps-Meddahi(2005) test for normality
    ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat15.cpp

    n<-length(x)
    varX <- statBM34 <- tmp3 <- tmp4 <- tmp5 <- tmp6 <- 0 
    z<- matrix(0,n,0)
    meanX <- mean(x)
    z <- scale(x)

    tmp3 <- sum((z^3 - 3 * z) / sqrt(6))
    tmp4 <- sum(((z^4 - 6 * z^2) + 3) / (2 * sqrt(6)))
    tmp5 <- sum((z^5 - 10 * z^3 + 15 * z) / (2 * sqrt(30)))
    tmp6 <- sum((z^6 - 15 * z^4 + 45 * z^2 - 15) / (12 * sqrt(5)))
    
    statBM36 <- (tmp3^2 + tmp4^2+tmp5^2 + tmp6^2)/n
    PVAL <- 1 - pchisq(statBM36, 2)
    
    result <- list(method = "Bontemps-Meddahi (2005), #2",
                   doi = "https://doi.org/10.1016/j.jeconom.2004.02.014",
                   statistic = statBM36,
                   df = 2,
                   p.value = PVAL)

    return(result)
}
