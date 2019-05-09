gel2008 <- function(x){
    ## The Gel-Gastwirth2008 test for normality
    ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat9.cpp

    meanX <-  mean(x)
    n <- length(x)
    m3<-0
    m4<-0
    Jn <- 0

    m3<- mean((x-meanX)^3)
    m4<- mean((x-meanX)^4)

    x <- sort(x)
    if (n%%2 == 0) {
        M<-(x[n/2+1]+x[n/2])/2
    }
    else{
        M<- x[n/2+1]
    }
    Jn <- sum(abs(x-M))
    Jn <- sqrt(pi/2) * Jn/n
    stat <- n*((m3/(Jn^3))^2)/6+n*((m4/(Jn^4)-3)^2)/64
    PVAL <- 1 - pchisq(stat, 2)
    
    result <- list(method = "Gel-Gastwirth (2008)",
                   doi = "https://doi.org/10.1016/j.econlet.2007.05.022",
                   statistic = stat,
                   df = 2,
                   p.value = PVAL)

    return(result)
}
