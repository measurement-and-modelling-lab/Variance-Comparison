bontemps2005a <- function(x){
    ## The 1st Bontemps-Meddahi(2005) test for normality
    ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat14.cpp
    
    n<-length(x)
    z <- scale(x)
    tmp3 <- sum((z^3 - 3 * z) / sqrt(6))
    tmp4 <- sum(((z^4 - 6 * z^2) + 3) / (2 * sqrt(6)))
    statBM34 <- (tmp3^2 + tmp4^2)/n
    PVAL <- 1 - pchisq(statBM34, 2)
    
    result <- list(method = "Bontemps-Meddahi (2005), #1",
                   doi = "https://doi.org/10.1016/j.jeconom.2004.02.014",
                   statistic = statBM34,
                   df = 2,
                   p.value = PVAL)

    return(result)
}
