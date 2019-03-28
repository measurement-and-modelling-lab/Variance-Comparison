cabana1994b <- function(x){
    ## The 2nd Cabana-Cabana(1994) test for normality
    ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat20.cpp
    ## bilateral test that rejects H0 only for large values of the test statistic
    
    n <- length(x)
    varX <- meanX <- sdX <- H3tilde <- H4tilde <- H5tilde <- H6tilde <- H7tilde <- H8tilde <- 0
    z <- H0 <- H1 <- H2 <- H3 <- H4 <- H5 <- H6 <- H7 <- H8 <- matrix(0,n,0)
    vectoraux2 <- matrix(0,n,0)
    meanX <- mean(x)
    for(i in 1:n){
        varX <- varX + x[i]^2  
    }
    varX <- n*(varX/n - meanX^2)/(n-1)
    sdX <-  sqrt(varX)
    z <- scale(x)
    
    H0 <- rep(1, n)
    H1 <- z
    H2 <- (z^2 - 1)/sqrt(2)
    H3 <- (z^3 - 3*z)/sqrt(6)
    H4 <- (z^4 - 6*z^2 + 3)/(2*sqrt(6))
    H5 <- (z^5 - 10*z^3 + 15*z)/(2*sqrt(30))
    H6 <- (z^6 - 15*z^4 + 45*z^2 - 15)/(12*sqrt(5))
    H7 <- (z^7 - 21*z^5 + 105*z^3 - 105*z)/(12*sqrt(35))
    H8 <- (z^8 - 28*z^6 + 210*z^4 - 420*z^2 + 105)/(24*sqrt(70))
    
    H3tilde <- sum(H3) / sqrt(n)
    H4tilde <- sum(H4) / sqrt(n) 
    H5tilde <- sum(H5) / sqrt(n)
    H6tilde <- sum(H6) / sqrt(n)
    H7tilde <- sum(H7) / sqrt(n)
    H8tilde <- sum(H8) / sqrt(n)
    
    vectoraux2 = (sqrt(2/1)*H0 + H2)*H5tilde + (sqrt(3/2)*H1 + H3)*H6tilde + (sqrt(4/3)*H2 + H4)*H7tilde + (sqrt(5/4)*H3 + H5)*H8tilde + (sqrt(5/4)*H3 + H5)*H8tilde
    
    statTK1 <-  abs(-dnorm(z[1])*H3tilde +(pnorm(z[1])-z[1]*dnorm(z[1]))*H4tilde - dnorm(z[1])*vectoraux2[1])

    for (i in 2:n) {
        tmp <- abs(-dnorm(z[i])*H3tilde +(pnorm(z[i])-z[i]*dnorm(z[i]))*H4tilde - dnorm(z[i])*vectoraux2[i])
        if (statTK1 < tmp){
            statTK1 <- tmp
        } 
    }
    
    result <- list(method = "Cabana-Cabana(1994), #2",
                   doi = "https://doi.org/10.1214/aos/1176325636",
                   statistic = statTK1,
                   df = NA,
                   p.value = NA)
    return(result)
}
