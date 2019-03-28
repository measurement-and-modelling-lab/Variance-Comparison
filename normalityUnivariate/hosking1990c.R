hosking1990c <- function(x){
    ## The 3rd Hosking test for normality (1990)
    ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat12.cpp
    
    pstarmod2 <- function(r,n,i){
        res <- 0
        for (k in 0:(r-1)) {
            res <- res + (-1)^k * choose(r-1,k) * choose(i-1,r+1-k) * choose(n-i,2+k)
            
        }
        
        return(res)
        
    }
    x <- sort(x)
    n <- length(x)
    l22<-l32<-l42<-0
    for(i in 3:n){
        l22 <- l22 + x[i-1]*pstarmod2(2,n,i-1)
        l32 <- l32 + x[i-1]*pstarmod2(3,n,i-1)
        l42 <- l42 + x[i-1]*pstarmod2(4,n,i-1)
        
    }
    l22 <- l22/(2*choose(n,6))
    l32 <- l32/(3*choose(n,7))
    l42 <- l42/(4*choose(n,8))
    
    tau32 <- l32/l22
    tau42 <- l42/l22
    
    if (1<=n & n<=25) {
        mutau42 <- 0.044174
        vtau32 <- 0.0086570
        vtau42 <- 0.0042066
    }
    
    if (25<n & n<=50) {
        mutau42 <- 0.040389
        vtau32 <- 0.0033818
        vtau42 <- 0.0013301
    }
    
    if (50<n) {
        mutau42 <- 0.039030
        vtau32 <- 0.0015120
        vtau42 <- 0.00054207
    }
    
    statTLmom2 <- tau32^2 / vtau32 + (tau42 - mutau42)^2 / vtau42
    PVAL <- 1 - pchisq(statTLmom2, 2)
    
    result <- list(method = "Hosking (1990), #3",
                   doi = "https://doi.org/10.1111/j.2517-6161.1990.tb01775.x",
                   statistic = statTLmom2,
                   df = 2,
                   p.value = PVAL)
    return(result)
    
}
