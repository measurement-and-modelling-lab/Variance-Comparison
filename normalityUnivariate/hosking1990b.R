hosking1990b <- function(x){
    ## The 2nd Hosking test for normality(1990)
    ## https://doi.org/10.1111/j.2517-6161.1990.tb01775.x
    ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat11.cpp
    
    pstarmod1 <- function(r,n,i){
        res <- 0
        for (k in 0:(r-1)) {
            res <- res + (-1)^k * choose(r-1,k) * choose(i-1,r-k) * choose(n-i,1+k)
            
        }
        
        return(res)
        
    }
    x <- sort(x)
    n <- length(x)
    l21<-l31<-l41<-0
    for(i in 3:n){
        l21 <- l21 + x[i-1]*pstarmod1(2,n,i-1)
        l31 <- l31 + x[i-1]*pstarmod1(3,n,i-1)
        l41 <- l41 + x[i-1]*pstarmod1(4,n,i-1)
        
    }
    l21 <- l21/(2*choose(n,4))
    l31 <- l31/(3*choose(n,5))
    l41 <- l41/(4*choose(n,6))
    
    tau31 <- l31/l21
    tau41 <- l41/l21
    
    if (1<=n & n<=25) {
        mutau41 <- 0.067077
        vtau31 <- 0.0081391
        vtau41 <- 0.0042752
    }
    
    if (25<n & n<=50) {
        mutau41 <- 0.064456
        vtau31 <- 0.0034657
        vtau41 <- 0.0015699
    }
    
    if (50<n) {
        mutau41 <- 0.063424
        vtau31 <- 0.0016064
        vtau41 <- 0.00068100
    }
    
    statTLmom1 <- tau31^2 / vtau31 + (tau41 - mutau41)^2 / vtau41
    PVAL <- 1 - pchisq(statTLmom1, 2)
    
    result <- list(method = "Hosking (1990), #2",
                   doi = "https://doi.org/10.1111/j.2517-6161.1990.tb01775.x",
                   statistic = statTLmom1,
                   df = 2,
                   p.value = PVAL)
    return(result)
    
    
    
}
